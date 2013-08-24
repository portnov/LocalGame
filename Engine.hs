
module Engine where

import Control.Monad
import Control.Monad.State
import Data.List
import Text.Printf

import Cards
import Types

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) fn = fn x

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
      setHand i $ card: hand
      modify $ \st -> st {deck = newDeck}

initGame :: Int -> Int -> Game ()
initGame n handSize = do
  pack <- lift $ shuffle fullPack
  modify $ \st -> st {deck = pack}
  replicateM_ handSize $ do
    forM_ [0..n-1] $ \playerIdx -> do
      giveCard playerIdx
  (card:newDeck) <- gets deck
  modify $ \st -> st {deck = newDeck, trash = [card]}

emptyState :: [Player] -> GameState
emptyState players = GS {
  hands = replicate (length players) [],
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
  if card `elem` hand
    then do
         setHand i (delete card hand)
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
            (_,card) = meldCards meld !! jokerIdx
        return (meld', card)

changeJoker i (clr, meldId) = do
  hand <- getHand i
  case findIndex (== Joker clr) hand of
    Nothing -> fail $ printf "No %s joker in hand" (show clr)
    Just jokerIdx -> do
      meld <- getMeld meldId
      (meld', card) <- meldChangeJoker meld clr
      setMeld meldId meld'
      setHand i $ filter (/= Joker clr) hand ++ [card]

pickTrash i n = do
  hand <- getHand i
  (newHand, newTrash) <- gets $ splitAt n . trash
  setHand i $ hand ++ newHand
  modify $ \st -> st {trash = newTrash}

newMeld i meld = do
  hand <- getHand i
  let cards = map snd $ meldCards meld
  if all (`elem` hand) cards
    then do
      setHand i $ hand \\ cards
      nmelds <- gets (length . melds)
      let meld' = meld {meldId = nmelds}
      modify $ \st -> st {melds = meld' : melds st}
    else fail $ "No cards in hand: " ++ show (cards \\ hand)

addToMeld i (card, meldId) = do
  hand <- getHand i
  if card `elem` hand
    then do
      meld <- getMeld meldId
      if card `elem` meldAllowedToAdd meld
        then do
          setHand i $ delete card hand
          player <- getPlayer i
          meld' <- meldAdd player meld card
          nmelds <- gets (length . melds)
          let meld' = meld {meldId = nmelds}
          modify $ \st -> st {melds = meld' : melds st}
        else fail $ printf "Not allowed to add %s to meld #%d" (show card) meldId
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



