{-# LANGUAGE TypeFamilies #-}

module AI where

import Math.Combinat.Sets
import Data.Maybe

import Cards
import Types
import Parser (readCards)

goodSublists :: [a] -> [[a]]
goodSublists xs = concat [kSublists k xs | k <- [3..13]]

data AI = AI Int
  deriving (Eq)

instance Show AI where
  show (AI i) = "AI #" ++ show i

instance IsPlayer AI where
  type PlayerState AI = ()
  playerName ai = show ai
  playerSelectMove _ = fail $ "Not implemented yet"

ai0 :: Player
ai0 = Player (AI 0) ()

possibleMelds :: Player -> Hand -> [Meld]
possibleMelds p hand =
  let subs = goodSublists hand
  in  catMaybes $ map (buildMeld p) subs

