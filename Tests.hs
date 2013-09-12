{-# LANGUAGE TypeSynonymInstances, PatternGuards #-}

module Tests where

import Control.Monad
import Control.Failure
import Test.QuickCheck
import Data.List

import Cards
import qualified CardSet as C
import Types
import Engine

newtype CardList = CardList [Card]
  deriving (Eq, Show)

instance Arbitrary CardList where
  arbitrary = do
    n <- choose (3,10)
    cs <- replicateM n arbitrary
    return $ CardList $ nub $ sort cs

instance Arbitrary Suit where
  arbitrary = elements [Clubs, Diamonds, Hearts, Spades]

instance Arbitrary CardValue where
  arbitrary = elements $ map N [2..10] ++ [Jack, Queen, King, Ace]

instance Arbitrary Card where
  arbitrary = elements fullPack
