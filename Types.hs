{-# LANGUAGE ExistentialQuantification, TypeFamilies, GADTs, TypeOperators, FlexibleContexts, DeriveDataTypeable #-}

module Types where

import Control.Monad
import Control.Monad.State
import Control.Failure
import Data.Functor
import Text.Printf
import Data.List
import Data.Maybe
import Data.Generics

import Cards
import qualified CardSet as C

thisPackType = PackType 2 True

fullPack = orderedPack thisPackType

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

buildStreet :: (Monad m, Failure String m) => Player -> Suit -> [CardColor] -> [CardValue] -> m Meld 
buildStreet _ _ jokers cards | length jokers + length cards < 3 = failure $ "Meld should have at least 3 cards"
buildStreet player ssuit jokers nojokers = do
    if length nojokers < 2 || length jokers > 1
      then failure "Meld should have at least 2 non-jokers and maximum 1 joker"
      else run
  where
    jokerColor = case jokers of
                   [] -> error "Unexpected: no joker"
                   [c] -> c
                   _ -> error "Unexpected: too many jokers"
    run = do
      let njokers = length jokers
      let sorted = sort $ map fromEnum nojokers
      let diffs = zipWith (-) (tail sorted) sorted
      let ncards = length jokers + length nojokers
      if njokers == 0
        then if any (> 1) diffs
               then failure $ "There are gaps in street and there are no any jokers"
               else return $ Street {
                               meldId = 0,
                               streetSuit = ssuit,
                               streetFrom = toEnum $ minimum sorted,
                               streetTo   = toEnum $ maximum sorted,
                               meldOwners = replicate ncards player,
                               meldJokers = replicate ncards Nothing }
        else -- if njokers == 1
          if any (> 2) diffs
            then failure $ "Too large gaps in street"
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
                   else failure $ "Cant' fill gaps in street with jokers"

buildAvenue :: (Monad m, Failure String m) => Player -> CardValue -> [CardColor] -> [Suit] -> m Meld
buildAvenue _ _ jokers cards | length jokers + length cards < 3 = failure $ "Meld should have at least 3 cards"
buildAvenue player cvalue [] suits = do
      let ncards = length suits
      return $ Avenue {
                 meldId = 0,
                 avenueValue = cvalue,
                 avenueSuits = suits,
                 meldOwners  = replicate ncards player,
                 meldJokers  = replicate ncards Nothing }
buildAvenue player cvalue [jokerColor] suits = do
      let ncards = length suits
      js <- case [Clubs, Diamonds, Hearts, Spades] \\ suits of
              [] -> failure $ "All suits are used, no place for joker"
              (s:_) -> return s
      return $ Avenue {
                 meldId = 0,
                 avenueValue = cvalue,
                 avenueSuits = js : suits,
                 meldOwners  = replicate ncards player,
                 meldJokers  = Just jokerColor : replicate (length suits) Nothing }

buildMeld :: (Monad m, Failure String m) => Player -> [Card] -> m Meld
buildMeld _ cards | length cards < 3 = failure $ "Meld should have at least 3 cards"
buildMeld player cards = do
    let (jokers, nojokers) = partition isJoker cards
        njokers = length jokers
    if length nojokers < 2 || njokers > 1
      then failure "Meld should have at least 2 non-jokers and maximum 1 joker"
      else do if allSame suit nojokers
                then buildStreet njokers nojokers
                else if allSame value nojokers
                       then buildAvenue njokers nojokers
                       else failure $ "These cards do not form a meld"
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
               then failure $ "There are gaps in street and there are no any jokers"
               else return $ Street {
                               meldId = 0,
                               streetSuit = suit (head nojokers),
                               streetFrom = toEnum $ minimum sorted,
                               streetTo   = toEnum $ maximum sorted,
                               meldOwners = replicate (length cards) player,
                               meldJokers = replicate (length cards) Nothing }
        else -- if njokers == 1
          if any (> 2) diffs
            then failure $ "Too large gaps in street"
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
                   else failure $ "Cant' fill gaps in street with jokers"

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
              [] -> failure $ "All suits are used, no place for joker"
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

type Game a = StateT GameState IO a

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
  playerSelectMove :: p -> Game Move
  onMove :: p -> Player -> Move -> Game ()
  onMove _ _ _ = return ()

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
    ChangeJoker CardColor MeldId
  | PickTrash Int
  | NewMeld Meld
  | AddToMeld Card MeldId
  | Trash Card
  deriving (Eq)

instance Show MoveAction where
  show (ChangeJoker clr i) = printf "Change %s Joker from meld #%d" (show clr) i
  show (PickTrash n) = printf "Pick last %d cards from trash" n
  show (NewMeld meld) = "Create new meld: " ++ show meld
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

buildMove :: (Monad m, Failure String m) => [MoveAction] -> m Move
buildMove mas =
  case [c | Trash c <- mas] of
    [] -> failure $ "One card must be trashed"
    [_] -> return $ foldl add emptyMove mas
    _ -> failure $ "Only one card can be trashed"
  where
    add move (ChangeJoker clr i) = move {toChangeJoker = Just (clr, i)}
    add move (PickTrash n) = move {toPickTrash = Just n}
    add move (NewMeld m) = move {toNewMelds = m : toNewMelds move}
    add move (AddToMeld card i) = move {toAddToMelds = (card,i) : toAddToMelds move}
    add move (Trash card) = move {toTrash = card}

    emptyMove = Move Nothing Nothing [] [] (Joker Red)

