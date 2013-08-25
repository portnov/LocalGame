{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}

module AI where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Generics
import Text.Printf

import Cards
import Types
import Parser (readCards)
import Engine

data AI = AI {
    aiId :: Int,
    aiKnownCards :: M.Map String Hand }
  deriving (Eq, Typeable)

instance Show AI where
  show (AI i _) = "AI #" ++ show i

instance IsPlayer AI where
  playerName ai = show ai
  playerIdx (AI i _) = i

  playerSelectMove me@(AI i knownCards) = do
    hand <- getHand i
    lift $ putStrLn $ printf "AI#%d has %d cards in hand." i (length hand)
    moves <- validMoves (Player me) hand
    if null moves
      then fail "Unexpected: no moves."
      else do
--             lift $ print moves
            rs <- mapM (evalGame . evalMove i) moves
            newPoints <- zipWithM (movePoints i knownCards) moves rs
            let maxPoints = maximum newPoints
                Just moveIdx = findIndex (== maxPoints) newPoints
                move = moves !! moveIdx
            lift $ putStrLn $ "AI move: " ++ show move
            return move


  onMove me@(AI i st) player@(Player p) move = do
    if playerName p == playerName me
      then return ()
      else do
           whenJust (toPickTrash move) (onPickTrash i player)
           onTrash i player (toTrash move)

movePoints i _ move Nothing = fail $ "Unexpected: invalid move generated: " ++ show move
movePoints i knownCards move (Just st) = do
    let newPoints = myPoints i st
    t <- gets trash
    let newTrash = toTrash move : t
    let otherCards = M.elems knownCards
    ps <- gets players
    let otherPoints = [go player (newTrash ++ hand) | (player, hand) <- zip ps otherCards]
    let maxOtherPoints = if null otherPoints
                           then 0
                           else maximum otherPoints
    return $ newPoints - maxOtherPoints
  where
    go p hand = sum $ map eval $ possibleMelds p hand
    eval meld = sum $ map meldPoints $ map snd $ meldCards' meld

modifyMe :: Int -> (AI -> AI) -> Game ()
modifyMe i fn = do
  Player p <- getPlayer i
  case cast p of
    Just me@(AI {}) -> setPlayer i $ Player (fn me)
    Nothing -> fail $ printf "Player #%d is not AI!" i

ai :: Int -> Player
ai i = Player (AI i M.empty)

addKnownCards :: String -> [Card] -> AI -> AI
addKnownCards p cards st = st { aiKnownCards = M.insertWith (++) p cards (aiKnownCards st) }

dropKnownCard :: String -> Card -> AI -> AI
dropKnownCard p card st = st { aiKnownCards = M.update (Just . delete card) p (aiKnownCards st) }

onPickTrash :: Int -> Player -> Int -> Game ()
onPickTrash me (Player p) n = do
  cards <- gets (take n . trash)
  modifyMe me $ addKnownCards (playerName p) cards

onTrash :: Int -> Player -> Card -> Game ()
onTrash me (Player p) card = do
  modifyMe me $ dropKnownCard (playerName p) card

