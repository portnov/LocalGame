{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Parser where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.State
import Text.Parsec
import Text.Parsec.String
import Control.Failure
import Data.Functor.Identity
import Data.Char
import Text.Localize

import Cards
import Types

instance Failure String Identity where
  failure s = fail s

instance Failure LocalizedString Identity where
  failure s = fail $ show s

pSuit :: Parser Suit
pSuit = do
  s <- letter
  case toUpper s of
    'C' -> return Clubs
    'D' -> return Diamonds
    'H' -> return Hearts
    'S' -> return Spades
    _ -> fail $ "Unknown suit: " ++ [s]

pValue :: Parser CardValue
pValue = try numeric <|> icon
  where
    numeric = do
      ns <- many1 digit
      let n = read ns
      if 1 < n && n < 11
        then return (N n)
        else fail $ "Invalid card value: " ++ ns

    icon = do
      s <- letter
      case toUpper s of
        'J' -> return Jack
        'Q' -> return Queen
        'K' -> return King
        'A' -> return Ace
        _ -> fail $ "Unknown icon: " ++ [s]

pCard :: Parser Card
pCard = try joker <|> usualCard
  where
    joker = try (string "RJ" >> return (Joker Red)) <|>
                (string "BJ" >> return (Joker Black))

    usualCard = do
      v <- pValue
      s <- pSuit
      return (Card s v)

moveAction :: Player -> Parser MoveAction
moveAction p =
      try pChangeJoker
  <|> try pPickTrash
  <|> try (pNewMeld p)
  <|> try (pAddToMeld p)
  <|> pTrash

pChangeJoker :: Parser MoveAction
pChangeJoker = do
  string "change"
  spaces
  clr <- pColor
  spaces
  i <- number
  return (ChangeJoker clr i Nothing)

pColor :: Parser CardColor
pColor = try (string "RJ" >> return Red) <|> (string "BJ" >> return Black)

number :: Parser Int
number = do
  ns <- many1 digit
  return (read ns)

pPickTrash :: Parser MoveAction
pPickTrash = do
  string "pick"
  spaces
  n <- number
  return (PickTrash n)

pNewMeld :: Player -> Parser MoveAction
pNewMeld p = do
  string "meld"
  spaces
  cards <- pCard `sepEndBy` spaces
  meld <- buildMeld p cards
  return $ NewMeld meld

pAddToMeld :: Player -> Parser MoveAction
pAddToMeld p = do
  string "add"
  spaces
  c <- pCard
  spaces
  n <- number
  return $ AddToMeld c n

pTrash :: Parser MoveAction
pTrash = do
  string "trash"
  spaces
  c <- pCard
  return (Trash c)

moveActions :: Player -> Parser [MoveAction]
moveActions p = moveAction p `sepEndBy1` (char ';' >> spaces)

dummy :: Player
dummy = Player (Dummy 0)

readCards :: String -> Either String [Card]
readCards str =
  case runParser (pCard `sepEndBy` spaces) () "<input>" str of
    Right res -> return res
    Left e -> failure $ show e

