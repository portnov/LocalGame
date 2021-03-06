{-# LANGUAGE ExistentialQuantification, TypeFamilies, GADTs, TypeOperators, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Types where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Failure
import Data.Functor
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Text.Printf
import Data.List
import Data.Maybe
import Data.Generics
import Data.Monoid
import Data.Text.Format
import Text.Localize

import Cards
import qualified CardSet as C

thisPackType = PackType 2 True

fullPack = orderedPack thisPackType

sortDiff :: Ord a => (a -> a -> Maybe Bool) -> [a] -> Maybe [a]
sortDiff _ [] = Just []
sortDiff diff list = go [] list
  where
    go acc [] = Just acc
    go [] xs = let m = maximum xs
               in  go [m] (delete m xs)
    go acc xs =
      case find (\x -> diff x (last acc) == Just True) xs of
        Just x -> go (acc ++ [x]) (delete x xs)
        Nothing -> case find (\x -> diff x (head acc) == Just False) xs of
                     Just x -> go (x:acc) (delete x xs)
                     Nothing -> Nothing
      
diffC :: Int -> Int -> Maybe Bool
diffC 14 2 = Just False
diffC 2 14 = Just True
diffC 13 2 = Just False
diffC 2 13 = Just True
diffC 14 3 = Just False
diffC 3 14 = Just True
diffC x y
  | y-x == 1 = Just False
  | y-x == 2 = Just False
  | x-y == 1 = Just True
  | x-y == 2 = Just True
  | otherwise = Nothing

type Hand = C.CardSet
type Deck = [Card]
type Trash = [Card]
type MeldId = Int

data Meld =
    Street {
      meldId :: MeldId,
      streetSuit :: Suit,
      streetFrom :: CardValue,
      streetTo :: CardValue,
      meldOwners :: [Player],
      meldJokers :: [Maybe CardColor]}
  | Avenue {
      meldId :: MeldId,
      avenueValue :: CardValue,
      avenueSuits :: [Suit],
      meldOwners :: [Player],
      meldJokers :: [Maybe CardColor]}
  deriving (Eq)

meldCards :: Meld -> [(Player, Card)]
meldCards (Street _ suit from to owners jokers) = zip owners $ zipWith card jokers [from .. to]
  where
    card (Just clr) _ = Joker clr
    card Nothing v = Card suit v
meldCards (Avenue _ val suits owners jokers) = zip owners $ zipWith card jokers suits
  where
    card (Just clr) _ = Joker clr
    card Nothing suit = Card suit val

meldCards' :: Meld -> [(Player, Card)]
meldCards' (Street _ suit from to owners _) = zip owners $ map (Card suit) [from..to]
meldCards' (Avenue _ val suits owners _) = zip owners [Card suit val | suit <- suits]

meldGetJokers :: Meld -> [(CardColor, Card)]
meldGetJokers (Street _ suit from to _ jokers) = catMaybes $ zipWith card jokers [from..to]
  where
    card Nothing _ = Nothing
    card (Just clr) val = Just (clr, Card suit val)
meldGetJokers (Avenue _ val suits _ jokers) = catMaybes $ zipWith card jokers suits
  where
    card Nothing _ = Nothing
    card (Just clr) suit = Just (clr, Card suit val)

allSame :: (Eq b) => (a -> b) -> [a] -> Bool
allSame fn xs = and $ zipWith (==) ys (tail ys)
  where
    ys = map fn xs

buildStreet :: (Monad m, Failure LocalizedString m) => Player -> Suit -> [CardColor] -> [CardValue] -> m Meld 
buildStreet _ _ jokers cards | length jokers + length cards < 3 = failure $ __ "Meld should have at least 3 cards"
buildStreet player ssuit jokers nojokers = do
    if length nojokers < 2 || length jokers > 1
      then failure $ __ "Meld should have at least 2 non-jokers and maximum 1 joker"
      else run
  where
    jokerColor = case jokers of
                   [] -> error "Unexpected: no joker"
                   [c] -> c
                   _ -> error "Unexpected: too many jokers"
    run = do
      let njokers = length jokers
      let sorted = sort $ map fromEnum nojokers
--       sorted <- case sortDiff diffC (sort $ map fromEnum nojokers) of
--                   Nothing -> failure $ "Cards do not form a street"
--                   Just s -> return s
      let diffs = zipWith (-) (tail sorted) sorted
      let ncards = length jokers + length nojokers
      if njokers == 0
        then if any (> 1) diffs
               then failure $ __ "There are gaps in street and there are no any jokers"
               else return $ Street {
                               meldId = 0,
                               streetSuit = ssuit,
                               streetFrom = toEnum $ minimum sorted,
                               streetTo   = toEnum $ maximum sorted,
                               meldOwners = replicate ncards player,
                               meldJokers = replicate ncards Nothing }
        else -- if njokers == 1
          if any (> 2) diffs
            then failure $ __ "Too large gaps in street"
            else do
                 let ngaps = length (filter (> 1) diffs)
                 if ngaps <= njokers
                   then if ngaps == 1
                          then do
                            let Just i = findIndex (== 2) diffs
                                jokerIdx = i+1
                                jokers = replicate jokerIdx Nothing ++ [Just jokerColor] ++
                                         replicate (length nojokers - jokerIdx) Nothing
                            return $ Street {
                                       meldId = 0,
                                       streetSuit = ssuit,
                                       streetFrom = toEnum $ minimum sorted,
                                       streetTo   = toEnum $ maximum sorted,
                                       meldOwners = replicate ncards player,
                                       meldJokers = jokers }
                          else do
                            if maximum sorted == fromEnum Ace
                              then return $ Street {
                                       meldId = 0,
                                       streetSuit = ssuit,
                                       streetFrom = toEnum $ minimum sorted - 1,
                                       streetTo   = toEnum $ maximum sorted,
                                       meldOwners = replicate ncards player,
                                       meldJokers = Just jokerColor : replicate (length nojokers) Nothing }
                              else return $ Street {
                                       meldId = 0,
                                       streetSuit = ssuit,
                                       streetFrom = toEnum $ minimum sorted,
                                       streetTo   = toEnum $ maximum sorted + 1,
                                       meldOwners = replicate ncards player,
                                       meldJokers = replicate (length nojokers) Nothing ++ [Just jokerColor] }
                   else failure $ __ "Cant' fill gaps in street with jokers"

buildAvenue :: (Monad m, Failure LocalizedString m) => Player -> CardValue -> [CardColor] -> [Suit] -> m Meld
buildAvenue _ _ jokers cards | length jokers + length cards < 3 = failure $ __ "Meld should have at least 3 cards"
buildAvenue player cvalue [] suits = do
      let ncards = length suits
      return $ Avenue {
                 meldId = 0,
                 avenueValue = cvalue,
                 avenueSuits = suits,
                 meldOwners  = replicate ncards player,
                 meldJokers  = replicate ncards Nothing }
buildAvenue player cvalue [jokerColor] suits = do
      let ncards = length suits + 1
      js <- case [Clubs, Diamonds, Hearts, Spades] \\ suits of
              [] -> failure $ __ "All suits are used, no place for joker"
              (s:_) -> return s
      return $ Avenue {
                 meldId = 0,
                 avenueValue = cvalue,
                 avenueSuits = js : suits,
                 meldOwners  = replicate ncards player,
                 meldJokers  = Just jokerColor : replicate (length suits) Nothing }
buildAvenue _ _ jokers _ = failure $ __ "Meld should have maximum 1 joker"

buildMeld :: (Monad m, Failure LocalizedString m) => Player -> [Card] -> m Meld
buildMeld _ cards | length cards < 3 = failure $ __ "Meld should have at least 3 cards"
buildMeld player cards = do
    let (jokers, nojokers) = partition isJoker cards
        njokers = length jokers
    if length nojokers < 2 || njokers > 1
      then failure $ __ "Meld should have at least 2 non-jokers and maximum 1 joker"
      else do if allSame suit nojokers
                then buildStreet njokers nojokers
                else if allSame value nojokers
                       then buildAvenue njokers nojokers
                       else failure $ __ "These cards do not form a meld"
  where
    jokerColor = case [clr | Joker clr <- cards] of
                   [] -> error "Unexpected: no joker"
                   [c] -> c
                   _ -> error "Unexpected: too many jokers"

    buildStreet njokers nojokers = do
      let sorted = sort $ map (fromEnum . value) nojokers
      let diffs = zipWith (-) (tail sorted) sorted
      if njokers == 0
        then if any (> 1) diffs
               then failure $ __ "There are gaps in street and there are no any jokers"
               else return $ Street {
                               meldId = 0,
                               streetSuit = suit (head nojokers),
                               streetFrom = toEnum $ minimum sorted,
                               streetTo   = toEnum $ maximum sorted,
                               meldOwners = replicate (length cards) player,
                               meldJokers = replicate (length cards) Nothing }
        else -- if njokers == 1
          if any (> 2) diffs
            then failure $ __ "Too large gaps in street"
            else do
                 let ngaps = length (filter (> 1) diffs)
                 if ngaps <= njokers
                   then if ngaps == 1
                          then do
                            let Just i = findIndex (== 2) diffs
                                jokerIdx = i+1
                                jokers = replicate jokerIdx Nothing ++ [Just jokerColor] ++
                                         replicate (length nojokers - jokerIdx) Nothing
                            return $ Street {
                                       meldId = 0,
                                       streetSuit = suit (head nojokers),
                                       streetFrom = toEnum $ minimum sorted,
                                       streetTo   = toEnum $ maximum sorted,
                                       meldOwners = replicate (length cards) player,
                                       meldJokers = jokers }
                          else do
                            if maximum sorted == fromEnum Ace
                              then return $ Street {
                                       meldId = 0,
                                       streetSuit = suit (head nojokers),
                                       streetFrom = toEnum $ minimum sorted - 1,
                                       streetTo   = toEnum $ maximum sorted,
                                       meldOwners = replicate (length cards) player,
                                       meldJokers = Just jokerColor : replicate (length nojokers) Nothing }
                              else return $ Street {
                                       meldId = 0,
                                       streetSuit = suit (head nojokers),
                                       streetFrom = toEnum $ minimum sorted,
                                       streetTo   = toEnum $ maximum sorted + 1,
                                       meldOwners = replicate (length cards) player,
                                       meldJokers = replicate (length nojokers) Nothing ++ [Just jokerColor] }
                   else failure $ __ "Cant' fill gaps in street with jokers"

    buildAvenue 0 nojokers = do
      return $ Avenue {
                 meldId = 0,
                 avenueValue = value (head nojokers),
                 avenueSuits = map suit nojokers,
                 meldOwners  = replicate (length cards) player,
                 meldJokers  = replicate (length cards) Nothing }
    buildAvenue 1 nojokers = do
      let suits = map suit nojokers
      js <- case [Clubs, Diamonds, Hearts, Spades] \\ suits of
              [] -> failure $ __ "All suits are used, no place for joker"
              (s:_) -> return s
      return $ Avenue {
                 meldId = 0,
                 avenueValue = value (head nojokers),
                 avenueSuits = js : suits,
                 meldOwners  = replicate (length cards) player,
                 meldJokers  = Just jokerColor : replicate (length nojokers) Nothing }

instance Show Meld where
  show meld = ("[" ++ show i ++ "]:\t") ++ (untabs $ map showCard cards) ++ "\n\t" ++
                                          (untabs $ map showPlayer cards)
    where
      cards = meldCards meld
      showPlayer (p,_) = show p
      untabs = intercalate "\t"
      showCard (_,c) = "[" ++ show c ++ "]"

      i = meldId meld

data GameState = GS {
    hands :: [Hand],
    deck :: Deck,
    trash :: Trash,
    melds :: [Meld],
    players :: [Player] }
  deriving (Eq)

instance Show GameState where
  show gs = "Hands:\n" ++ unwords (zipWith showHand [1..] (hands gs)) ++ "\n" ++
            "Melds:\n" ++ unwords (map showMeld (melds gs)) ++ "\n" ++
            "Trash: " ++ unwords (map show $ trash gs)
    where
      showHand i hand = show i ++ ":\t" ++ show hand ++ "\n"

showMeld m = show m ++ "\n"

instance E.Exception String
instance E.Exception LocalizedString

instance Error LocalizedString where
  noMsg = mempty
  strMsg msg = Untranslated $ T.pack msg

newtype Game a = Game {unGame :: ErrorT LocalizedString (StateT GameState IO) a}
  deriving (Monad,MonadError LocalizedString,MonadState GameState,MonadIO)

instance Failure LocalizedString Game where
  failure e = Game $ ErrorT $ return $ Left e

getHand :: Int -> Game Hand
getHand i = do
  hs <- gets hands
  if i <= length hs
    then return $ hs !! i
    else fail $ "No such player: " ++ show i

setHand :: Int -> Hand -> Game ()
setHand i hand = do
  hs <- gets hands
  if i <= length hs
    then modify $ \st -> st {hands = setelt i hand (hands st)}
    else fail $ "No such player: " ++ show i

setelt :: Int -> a -> [a] -> [a]
setelt i _ [] = error $ printf "setelt %d on empty list!" i
setelt 0 v (_:xs) = v:xs
setelt i v (x:xs)
  | i > 0 = x: setelt (i-1) v xs
  | otherwise = error $ "Unexpected: setelt " ++ show i

class Typeable p => IsPlayer p where
  playerName :: p -> String
  playerIdx :: p -> Int

  initPlayer :: p -> Game ()
  initPlayer _ = return ()

  beforeGiveCards :: p -> Game ()
  beforeGiveCards _ = return ()
  
  onInitialTrash :: p -> Card -> Game ()
  onInitialTrash _ _ = return ()

  onGiveCard :: p -> Card -> Game ()
  onGiveCard _ _ = return ()

  onEndGame :: p -> Player -> Game ()
  onEndGame _ _ = return ()

  playerSelectMove :: p -> Game Move

  beforeMove :: p -> Player -> Move -> Game ()
  beforeMove _ _ _ = return ()

  afterMove :: p -> Player -> Move -> Game ()
  afterMove _ _ _ = return ()

data Dummy = Dummy Int
  deriving (Typeable)

instance IsPlayer Dummy where
  playerName _ = "Dummy"
  playerIdx (Dummy i) = i
  playerSelectMove _ = fail $ "Move selection is not implemented for dummy player"

data Player = forall p. IsPlayer p => Player p 

instance Eq Player where
  (Player p1 ) == (Player p2) = playerName p1 == playerName p2

instance Show Player where  
  show (Player p) = playerName p

data MoveAction =
    ChangeJoker CardColor MeldId (Maybe Card)
  | PickTrash Int
  | NewMeld Meld
  | MeldCard Int Card
  | AddToMeld Card MeldId
  | Trash Card
  deriving (Eq)

instance Show MoveAction where
  show (ChangeJoker clr i _) = printf "Change %s Joker from meld #%d" (show clr) i
  show (PickTrash n) = printf "Pick last %d cards from trash" n
  show (NewMeld meld) = "Create new meld: " ++ show meld
  show (MeldCard i card) = printf "Try to put %s to new meld #%d" (show card) i
  show (AddToMeld card i) = printf "Add %s to meld #%d" (show card) i
  show (Trash card) = "Trash " ++ show card

data Move = Move {
  toChangeJoker :: Maybe (CardColor, MeldId),
  toPickTrash :: Maybe Int,
  toNewMelds :: [Meld],
  toAddToMelds :: [(Card, MeldId)],
  toTrash :: Card }
  deriving (Eq)

instance Show Move where
  show move = maybe "" showChangeJoker (toChangeJoker move) ++
              maybe "" showPick (toPickTrash move) ++
              unlines (map goMeld $ toNewMelds move) ++
              unlines (map goAdd $ toAddToMelds move) ++
              "\nTrash " ++ show (toTrash move)
    where
      showChangeJoker (clr, i) = printf "Change %s Joker from meld #%d\n" (show clr) i
      showPick n = printf "Pick last %d cards from trash\n" n
      goMeld meld = "New meld:\n" ++ show meld
      goAdd (card,i) = printf "Add %s to meld #%d" (show card) i

describeMove :: Move -> LocalizedString
describeMove move =
              maybe no showChangeJoker (toChangeJoker move) <>
              maybe no showPick (toPickTrash move) <>
              unlinesL (map goMeld $ toNewMelds move) <>
              unlinesL (map goAdd $ toAddToMelds move) <>
              __ "Trash " <> describeCard (toTrash move)
    where
      no = Untranslated (T.pack "")
      showChangeJoker (clr, i) = lprintf "Change {} Joker from meld #{}; " (show clr, i)
      showPick n = lprintf "Pick last {} cards from trash; " (Only n)
      goMeld meld = __ "New meld: " <> describeMeld meld <> (Untranslated $ T.pack "; ")
      goAdd (card,i) = __ "Add " <> describeCard card <> lprintf " to meld #{}; " (Only i)
      
      describeMeld meld =
        let cards = map snd $ meldCards meld
        in  unwordsL $ map describeCard cards

buildMove :: forall m. (Monad m, Failure LocalizedString m) => Player -> [MoveAction] -> m Move
buildMove player mas =
  case [c | Trash c <- mas] of
    [] -> failure $ __ "One card must be trashed"
    [_] -> do
           (newMeldCards, move) <- foldM add (M.empty, emptyMove) mas
           newMelds <- forM (M.elems newMeldCards) $ \cards ->
                         buildMeld player cards
           return $ move {toNewMelds = newMelds ++ toNewMelds move}

    _ -> failure $ __ "Only one card can be trashed"
  where
    add :: (M.Map Int [Card], Move) -> MoveAction -> m (M.Map Int [Card], Move)
    add (m,move) (ChangeJoker clr i _) = return $ (m, move {toChangeJoker = Just (clr, i)})
    add (m,move) (PickTrash n) = return $ (m, move {toPickTrash = Just n})
    add (m,move) (NewMeld meld) = return $ (m, move {toNewMelds = meld : toNewMelds move})
    add (m,move) (AddToMeld card i) = return $ (m, move {toAddToMelds = (card,i) : toAddToMelds move})
    add (m,move) (Trash card) = return $ (m, move {toTrash = card})
    add (m,move) (MeldCard i card) =
      let m' = M.insertWith (++) i [card] m
      in  return $ (m', move)

    emptyMove = Move Nothing Nothing [] [] (Joker Red)

