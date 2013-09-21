module Cards where

import Data.Monoid
import Text.Localize
import qualified Data.Text.Lazy as T
import System.Random

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord)

data CardColor = Black | Red
  deriving (Eq, Ord, Show)

instance Show Suit where
  show Clubs    = "♣"
  show Diamonds = "♦"
  show Hearts   = "♥"
  show Spades   = "♠"

showSuit :: Suit -> String
showSuit Clubs = "C"
showSuit Diamonds = "D"
showSuit Hearts = "H"
showSuit Spades = "S"

suitName :: Suit -> LocalizedString
suitName Clubs = __ "Clubs"
suitName Diamonds = __ "Diamonds"
suitName Hearts = __ "Hearts"
suitName Spades = __ "Spades"

data CardValue =
    N Int
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord)

instance Show CardValue where
  show (N n) = show n
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

describeValue :: CardValue -> LocalizedString
describeValue (N n) = Untranslated $ T.pack $ show n
describeValue Jack  = __ "Jack"
describeValue Queen = __ "Queen"
describeValue King  = __ "King"
describeValue Ace   = __ "Ace"

instance Enum CardValue where
  fromEnum (N n) = n
  fromEnum Jack = 11
  fromEnum Queen = 12
  fromEnum King = 13
  fromEnum Ace = 14

  toEnum i | i < 11 = N i
  toEnum 11 = Jack
  toEnum 12 = Queen
  toEnum 13 = King
  toEnum 14 = Ace
  toEnum n = error $ "Unexpected: invalid card value: " ++ show n

  enumFromTo from to
    | to >= from = map toEnum [fromEnum from .. fromEnum to]
    | otherwise = map toEnum $ [fromEnum from .. 14] ++ [2 .. fromEnum to]

instance Bounded CardValue where
  minBound = N 2
  maxBound = Ace

data Card =
    Card {
      suit :: Suit,
      value :: CardValue }
  | Joker CardColor
  deriving (Eq, Ord)

instance Show Card where
  show (Card s v) = show s ++ show v
  show (Joker Red)  = "RJ"
  show (Joker Black)  = "BJ"

describeCard :: Card -> LocalizedString
describeCard (Joker Red) = __ "Red Joker"
describeCard (Joker Black) = __ "Black Joker"
describeCard (Card suit value) = describeValue value <> __ " of " <> suitName suit

isJoker :: Card -> Bool
isJoker (Joker _) = True
isJoker _ = False

data PCard = Card :# Int

instance Eq PCard where
  (c :# _) == (d :# _) = c == d

instance Ord PCard where
  compare (c :# _) (d :# _) = compare c d

instance Show PCard where
  show (c :# n) = show c ++ " (" ++ show n ++ ")"

class GameCard g where
  compareCards :: g -> g -> Ordering
  showCard :: g -> String
  cardPoints :: g -> Int

instance GameCard Card where
  compareCards = compare
  showCard = show

  cardPoints (Card _ (N n)) = n
  cardPoints (Card _ Jack)  = 11
  cardPoints (Card _ Queen) = 12
  cardPoints (Card _ King)  = 13
  cardPoints (Card _ Ace)   = 14
  cardPoints x = error $ "Unexpected card: " ++ show x

(<#) :: GameCard a => a -> a -> Bool
a <# b = a `compareCards` b == LT

(>#) :: GameCard a => a -> a -> Bool
a ># b = a `compareCards` b == GT

(=#) :: GameCard a => a -> a -> Bool
a =# b = a `compareCards` b == EQ

data PackType = PackType {
    minimumValue :: Int,
    withJokers :: Bool }

suitColor :: Suit -> CardColor
suitColor Clubs    = Black
suitColor Diamonds = Red
suitColor Hearts   = Red
suitColor Spades   = Black

cardColor :: Card -> CardColor
cardColor = suitColor . suit

orderedPack :: PackType -> [Card]
orderedPack pack = concatMap allSuits (map N [minimumValue pack..10] ++ [Jack, Queen, King, Ace]) ++ mbJokers
  where
    mbJokers | withJokers pack = [Joker Red, Joker Black]
             | otherwise       = []

allSuits :: CardValue -> [Card]
allSuits v = [Card s v | s <- [Clubs, Diamonds, Hearts, Spades]]

nPacks :: Int -> PackType -> [PCard]
nPacks n pack = concat [map (:# i) $ orderedPack pack | i <- [1..n]]

shuffle :: [a] -> IO [a]
shuffle xs = selektion (length xs) xs
  where
    selektion :: Int -> [a] -> IO [a]
    selektion 0 xs = return  []
    selektion k xs = do
             i <- randomRIO (0, length xs - 1)
             let (here, y : there) = splitAt i xs
             ys <- selektion (pred k) $ here ++ there
             return $ y : ys
             
