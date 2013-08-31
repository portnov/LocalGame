{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Engine where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.State
import Math.Combinat.Sets
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Text.Printf
import System.IO
import System.Random

import Cards
import qualified CardSet as C
import Types

instance E.Exception String

exitBonus :: Int
exitBonus = 50

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) fn = fn x

concatFor :: Monad m => [a] -> (a -> m [b]) -> m [b]
concatFor xs fn = do
  ys <- sequence $ map fn xs
  return (concat ys)

handPoints :: Card -> Int
handPoints (Joker _) = 50
handPoints (Card _ v) = fromEnum v

meldPoints :: Card -> Int
meldPoints (Joker _) = error $ "Unpected: joker in meld"
meldPoints (Card _ v) = fromEnum v

getPoints :: Int -> Game Int
getPoints i = do
  hand <- getHand i
  player <- getPlayer i
  ms <- gets melds
  let allMelds = [c | (p,c) <- concat (map meldCards' ms), p == player]
      bonus = if C.null hand then exitBonus else 0
  return $ sum (map meldPoints allMelds) - C.sumMap handPoints hand + bonus

myPoints :: Int -> Int -> GameState -> Int
myPoints divider i st =
  let hand = hands st !! i
      player = players st !! i
      allMelds = [c | (p,c) <- concat (map meldCards' $ melds st), p == player]
  in  sum (map meldPoints allMelds) - (C.sumMap handPoints hand `div` divider)

giveCard :: Int -> Game ()
giveCard i = do
  st <- get
  cards <- gets deck
  case cards of
    [] -> do
          lift $ putStrLn "No more cards in deck."
          return ()
    (card:newDeck) -> do
      hand <- getHand i
      setHand i $ C.insert card hand
      lift $ putStrLn $ "Giving card to player #" ++ show i
      modify $ \st -> st {deck = newDeck}

initGame :: Int -> Int -> Game ()
initGame nPlayers handSize = do
    pack <- lift $ shuffle fullPack
    modify $ \st -> st {deck = pack}
    replicateM_ handSize $ do
      forM_ [0..nPlayers-1] $ \playerIdx -> do
        giveCard playerIdx
    trashOne
  where
    trashOne = do
      (card:newDeck) <- gets deck
      if isJoker card
        then do
             lift $ putStrLn "Will not put joker to trash..."
             i <- lift $ randomRIO (0, length newDeck - 1)
             let (here, there) = splitAt i newDeck
             modify $ \st -> st {deck = here ++ [card] ++ there}
             trashOne
        else modify $ \st -> st {deck = newDeck, trash = [card]}

emptyState :: [Player] -> GameState
emptyState players = GS {
  hands = replicate (length players) C.empty,
  deck = fullPack,
  trash = [],
  melds = [],
  players = players }

evalMove :: Int -> Move -> Game ()
evalMove playerIdx move = do
  whenJust (toChangeJoker move) (changeJoker playerIdx)
  whenJust (toPickTrash move) (pickTrash playerIdx)
  forM_ (toNewMelds move) (newMeld playerIdx)
  forM_ (toAddToMelds move) (addToMeld playerIdx)
  doTrash playerIdx (toTrash move)

getPlayer :: Int -> Game Player
getPlayer i = do
  hs <- gets players
  if i <= length hs
    then return $ hs !! i
    else fail $ "No such player: " ++ show i

setPlayer :: Int -> Player -> Game ()
setPlayer i player = do
  hs <- gets players
  if i <= length hs
    then modify $ \st -> st {players = setelt i player (players st)}
    else fail $ "No such player: " ++ show i

getMeld :: MeldId -> Game Meld
getMeld i = do
  hs <- gets melds
  if i <= length hs
    then return $ hs !! i
    else fail $ "No such meld: #" ++ show i

setMeld :: MeldId -> Meld -> Game ()
setMeld i meld = do
  hs <- gets melds
  if i <= length hs
    then modify $ \st -> st {melds = setelt i meld (melds st)}
    else fail $ "No such meld: #" ++ show i

doTrash i card = do
  hand <- getHand i
  if card `C.elem` hand
    then do
         setHand i (C.delete card hand)
         modify $ \st -> st {trash = card : trash st}
    else fail $ printf "Cannot trash %s: no such card in hand!" (show card)

meldAllowedToAdd :: Meld -> [Card]
meldAllowedToAdd (Street _ suit from to _ _) = down ++ up
  where
    down | from == N 2 = []
         | otherwise = [Card suit (pred from) ]

    up | to == Ace = []
       | otherwise = [Card suit (succ to) ]

meldAllowedToAdd (Avenue _ val suits _ _) =
    [Card suit val | suit <- [Clubs, Diamonds, Hearts, Spades] \\ suits]


meldChangeJoker :: Meld -> CardColor -> Game (Meld, Card)
meldChangeJoker meld clr =
  case findIndex (== Just clr) (meldJokers meld) of
    Nothing -> fail $ printf "No %s joker in meld" (show clr)
    Just jokerIdx -> do
        let meld' = meld {meldJokers = setelt jokerIdx Nothing (meldJokers meld)}
            (_,card) = meldCards' meld !! jokerIdx
        return (meld', card)

changeJoker i (clr, meldId) = do
  hand <- getHand i
  meld <- getMeld meldId
  (meld', card) <- meldChangeJoker meld clr
  setMeld meldId meld'
  setHand i $ C.insert (Joker clr) (C.delete card hand)

pickTrash i n = do
  hand <- getHand i
  (newHand, newTrash) <- gets $ splitAt n . trash
  setHand i $ C.insertAll newHand hand
  modify $ \st -> st {trash = newTrash}

newMeld i meld = do
  hand <- getHand i
  let cards = map snd $ meldCards meld
  if all (`C.elem` hand) cards
    then do
      setHand i $ C.deleteAll cards hand
      nmelds <- gets (length . melds)
      let meld' = meld {meldId = nmelds }
      modify $ \st -> st {melds = melds st ++ [meld']}
    else fail $ "No cards in hand: " ++ show (cards \\ C.toList hand)

addToMeld i (card, meldId) = do
  hand <- getHand i
  if card `C.elem` hand
    then do
      meld <- getMeld meldId
      if card `elem` meldAllowedToAdd meld
        then do
          setHand i $ C.delete card hand
          player <- getPlayer i
          meld' <- meldAdd player meld card
          setMeld meldId meld'
        else fail $ printf "Not allowed to add %s to meld #%d; only allowed are %s" (show card) meldId
                                                          (show $ meldAllowedToAdd meld)
    else fail $ printf "No %s in hand" (show card)

meldAdd :: Player -> Meld -> Card -> Game Meld
meldAdd player meld@(Street {}) card = do
  if suit card /= streetSuit meld
    then fail $ printf "Cannot add %s to street #%d: invalid card suit" (show card) (meldId meld)
    else if value card == pred (streetFrom meld)
           then return $ meld {
                           streetFrom = pred (streetFrom meld),
                           meldOwners = player : meldOwners meld,
                           meldJokers = Nothing : meldJokers meld }
           else if value card == succ (streetTo meld)
                  then return $ meld {
                                  streetTo = succ (streetTo meld),
                                  meldOwners = meldOwners meld ++ [player],
                                  meldJokers = meldJokers meld ++ [Nothing] }
                  else fail $ printf "Cannot add %s to street %d: invalid card value" (show card) (meldId meld)
meldAdd player meld@(Avenue {}) card = do
  if value card /= avenueValue meld
    then fail $ printf "Cannot add %s to avenue #%d" (show card) (meldId meld)
    else if suit card `elem` avenueSuits meld
           then fail $ printf "Cannot add %s to avenue #%d: this suit is already in meld" (show card) (meldId meld)
           else return $ meld {
                           avenueSuits = suit card : avenueSuits meld,
                           meldOwners = player : meldOwners meld,
                           meldJokers = Nothing : meldJokers meld }

newHandSize :: Move -> Hand -> Int
newHandSize move hand = 
  let allNewMelds = concat $ map meldCards (toNewMelds move)
      added = toAddToMelds move
      diffCardsNumber = fromMaybe 0 (toPickTrash move) -
                        length allNewMelds - length added - 1
  in  C.size hand + diffCardsNumber


checkMove :: Player -> Move -> Game ()
checkMove actor@(Player player) move = do
  t <- gets trash
  let allNewMelds = map snd $ concat $ map meldCards (toNewMelds move)
      added = map fst (toAddToMelds move)
  ms <- gets melds
  let myMelds = [ [ c | (p,c) <- meldCards' meld, p == actor ] | meld <- ms]
      canExit = any (>= 4) (map length myMelds)
  whenJust (toChangeJoker move) $ \(color,_) -> do
    when (Joker color `notElem` allNewMelds) $
      fail $ "You can change joker only to use it in one of new melds."
  whenJust (toPickTrash move) $ \n -> do
    let newCards = take n t
        lastCard = last newCards
    when (lastCard `notElem` (allNewMelds ++ added)) $
      fail $ "You can pick trash only to use it in new or existing melds."
  case toTrash move of
    Joker _ -> fail "You can not trash joker."
    _ -> return ()
  hand <- getHand (playerIdx player)
  let newSz = newHandSize move hand
  deckSz <- gets (length . deck)
  if newSz < 0
    then fail "Too many cards spended"
    else if newSz == 0 && not canExit && deckSz > 0
           then fail "You can not exit yet."
           else return ()

isMoveValid :: Player -> Move -> Game Bool
isMoveValid player move = do
  r <- atomicallyTry False (checkMove player move)
  when r $
      lift $ putStr "." >> hFlush stdout
  return r

isMoveValid' :: Player -> Move -> Game Bool
isMoveValid' player move = do
  r <- atomicallyTry True (checkMove player move)
  return r

atomicallyTry :: Bool -> Game () -> Game Bool
atomicallyTry toPrintExc action = do
  st <- get
  r <- lift $ E.try $ execStateT action st
  case r of
    Right st' -> do
                 put st'
                 return True
    Left err -> do
                when toPrintExc $
                    lift $ putStrLn $ "Error: " ++ show (err :: E.SomeException)
                put st
                return False

evalGame :: Game () -> Game (Maybe GameState)
evalGame action = do
  st <- get
  r <- lift $ E.try $ execStateT action st
  put st
  case r of
    Right st' -> return (Just st')
    Left err -> do
                lift $ putStrLn $ "Error: " ++ show (err :: E.SomeException)
                return Nothing

possibleJokerChanges :: Hand -> Game [(CardColor, MeldId)]
possibleJokerChanges hand = do
    ms <- gets melds
    return [(clr,i) | (card,clr,i) <- concatMap getJoker ms, card `C.elem` hand]
  where
    getJoker meld =
      case meldGetJokers meld of
        [] -> []
        [(color,card)] -> [(card, color, meldId meld)]
        _ -> error $ "Unexpected: more than one joker in meld #" ++ show (meldId meld)

possibleMelds :: Player -> C.CardSet -> [Meld]
possibleMelds p cs =
  let jokers = C.csJokers cs
      aSublists list = concat [kSublists k list | k <- [3,4]]
      sSublists list = concat [kSublists k list | k <- [3..13]]
      avenueLists = [(value, suits) | (value, suits) <- M.assocs (C.byValue cs), length suits >= 3]
      avenues = catMaybes $ concat [[buildAvenue p value jokers sublist | sublist <- aSublists suits]
                                    | (value, suits) <- avenueLists]
      streetLists = [(values, suit) | (suit, values) <- M.assocs (C.bySuit cs), S.size values >= 3]
      streets = catMaybes $ concat [[buildStreet p suit jokers sublist | sublist <- sSublists (S.toList values)]
                                    | (values, suit) <- streetLists]
  in  streets ++ avenues

possibleAddToMelds :: Hand -> Game [(Card, MeldId)]
possibleAddToMelds hand = do
    ms <- gets melds
    return $ concat [ [(card, meldId meld) | card <- meldAllowedToAdd meld, card `C.elem` hand] | meld <- ms]

possibleMoves :: Player -> Hand -> Game [Move]
possibleMoves actor@(Player player) hand = do
  st0 <- get
  jokerChanges <- possibleJokerChanges hand
  actions <- concatFor (Nothing : map Just jokerChanges) $ \change -> do
                r1 <- case change of
                        Nothing -> return (Just st0)
                        Just (color, meldId) -> evalGame $ changeJoker (playerIdx player) (color, meldId)
                actions <- case r1 of
                              Nothing -> fail $ "Unexpected: cannot change joker in meld"
                              Just st1 -> do
                                let hand1 = hands st1 !! playerIdx player
                                t <- gets trash
                                let possiblePickTrash = Nothing : map Just [1 .. length t - 1]
                                concatFor possiblePickTrash $ \pt -> do
                                  let newCards = case pt of
                                                   Just n -> take n t
                                                   Nothing -> []
                                      hand2 = C.insertAll newCards hand1
                                  let melds2 = possibleMelds actor hand2
                                  actions <- concatFor (sublists melds2) $ \melds -> do
                                              let diffCards = map snd $ concat $ map meldCards melds
                                              let hand3 = C.deleteAll diffCards hand2
                                              allAdds <- possibleAddToMelds hand3
--                                               lift $ putStrLn $ "AllAdds: " ++ show (sublists allAdds)
                                              actions <- concatFor (sublists allAdds) $ \adds -> do
                                                            let diffCards2 = map fst adds
                                                            let hand4 = C.deleteAll diffCards2 hand3
                                                            let trashes = [Trash card | card <- C.toList hand4]
--                                                             lift $ putStrLn $ "Trashes: " ++ show trashes
                                                            let addActions = [AddToMeld card i | (card,i) <- adds]
                                                            if null addActions
                                                              then return [(Nothing, trash) | trash <- trashes]
                                                              else return [(Just add, trash) | add <- addActions, trash <- trashes]
--                                               lift $ putStrLn $ "Actions 0: " ++ show actions
--                                               lift $ putStrLn $ "Melds: " ++ show melds
                                              let newMelds = map NewMeld melds
                                              let res = if null newMelds
                                                          then [(Nothing, add, trash) | (add, trash) <- actions]
                                                          else [(Just newMeld, add, trash) | (add, trash) <- actions, newMeld <- newMelds]
--                                               lift $ putStrLn $ "Res: " ++ show res
                                              return res
--                                   lift $ putStrLn $ "Actions 1: " ++ show actions
                                  let pick = case pt of
                                               Nothing -> Nothing
                                               Just n -> Just (PickTrash n)
                                  return [(pick, newMeld, add, trash) | (newMeld, add, trash) <- actions]
--                 lift $ putStrLn $ "Actions 2: " ++ show actions
                let changeJoker = case change of
                                    Nothing -> Nothing
                                    Just (color, meldId) -> Just (ChangeJoker color meldId)
                return [(changeJoker, pick, newMeld, add, trash) | (pick, newMeld, add, trash) <- actions]
--   lift $ putStrLn $ "Actions 3: " ++ show actions
  forM actions $ \(changeJoker, pick, newMeld, add, trash) -> do
    let actionList = (maybeToList changeJoker) ++
                     (maybeToList pick) ++ 
                     (maybeToList newMeld) ++
                     (maybeToList add) ++ [trash]
    buildMove actionList

validMoves :: Player -> Hand -> Game [Move]
validMoves player hand = do
  allMoves <- possibleMoves player hand
  filterM (isMoveValid player) allMoves

