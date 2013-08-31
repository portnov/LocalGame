{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}

module AI where

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Generics
import Text.Printf
import System.IO

import Cards
import qualified CardSet as C
import Types
import Engine

otherPointsDivider :: Int
otherPointsDivider = 4

potentialMeldsDivider :: Int
potentialMeldsDivider = 2

selfHandNobodyCanExitDivider :: Int
selfHandNobodyCanExitDivider = 2

selfHandOneCanExitDivider :: Int
selfHandOneCanExitDivider = 1

data AI = AI {
    aiId :: Int,
    aiKnownCards :: M.Map String [Card] }
  deriving (Eq, Typeable)

instance Show AI where
  show (AI i _) = "AI #" ++ show i

data TerminatingCondition = TC {
    tcMaxMoves :: Int,
    tcMaxPoints :: Int,
    tcMinPoints :: Int,
    tcMovesEdge :: Int }
  deriving (Eq, Show)

defaultTC :: TerminatingCondition
defaultTC = TC 2000 90 10 1000

instance IsPlayer AI where
  playerName ai = show ai
  playerIdx (AI i _) = i

  playerSelectMove me@(AI i knownCards) = do
      hand <- getHand i
      let totalKnownCards = sum $ map length $ M.elems knownCards
          nKnownCardPlayers = M.size knownCards
      lift $ putStrLn $ printf "AI#%d has %d cards in its hand. It knows %d cards of %d other players." i (C.size hand) totalKnownCards nKnownCardPlayers
      lift $ putStr $ "Generating list of possible moves: "
      lift $ hFlush stdout
      moves <- validMoves (Player me) hand
      let nMoves = length moves
      if null moves
        then if C.size hand == 1
               then do
                    lift $ putStrLn $ " single card, put it to trash and exit."
                    return $ Move {
                              toChangeJoker = Nothing,
                              toPickTrash = Nothing,
                              toNewMelds = [],
                              toAddToMelds = [],
                              toTrash = head (C.toList hand) }
               else fail "Unexpected: no moves."
        else do
              rs <- mapM go moves
              lift $ putStr $ printf " done.\nSelecting best of %d moves: " nMoves
              lift $ hFlush stdout
              currentSt <- get
              let currentPoints = myPoints 1 i currentSt
                  tc = if currentPoints >= 0
                         then defaultTC
                         else defaultTC {tcMinPoints = -currentPoints}
              newPointMs <- iter tc (points hand) $ zip3 [0..] moves rs
              let newPoints = map snd newPointMs
                  maxPoints = maximum newPoints
                  Just moveIdx = findIndex (== maxPoints) newPoints
                  move = fst $ newPointMs !! moveIdx
                  seenMoves = length newPointMs
              lift $ putStrLn $ printf "\nAI#%d selected move #%d (points %d): %s" i (seenMoves - moveIdx - 1) maxPoints (show move)
              return move
    where
      go move = do
        r <- evalGame $ evalMove i move
        return r

      points hand j move r = do
        p <- movePoints i hand knownCards move r
        lift $ putStr $ printf "[%d:%d]" (j :: Int) p
        lift $ hFlush stdout
        return p

      iter tc fn ys = iterGo [] tc fn ys

      checkTC tc p j = do
        if p >= tcMaxPoints tc
          then return True
          else if j >= tcMovesEdge tc && p >= tcMinPoints tc
                 then return True
                 else if j >= tcMaxMoves tc
                        then return True
                        else return False

      iterGo :: [(Move,Int)] -> TerminatingCondition
             -> (Int -> Move -> Maybe GameState -> Game Int)
             -> [(Int, Move, Maybe GameState)] -> Game [(Move, Int)]
      iterGo acc _ _ [] = return acc
      iterGo acc tc fn ((j, move, r):xs) = do
        p <- fn j move r
        end <- checkTC tc p j
        if end
          then return ((move,p):acc)
          else iterGo ((move,p):acc) tc fn xs
                 


  onMove me@(AI i _) player@(Player p) move = do
    if playerName p == playerName me
      then return ()
      else do
           whenJust (toPickTrash move) (onPickTrash i player)
           forM_ (toNewMelds move) $ \meld -> onNewMeld i player $ map snd $ meldCards meld
           forM_ (toAddToMelds move) (onAddToMeld i player)
           onTrash i player (toTrash move)

movePoints :: Int -> Hand -> M.Map String [Card] -> Move -> Maybe GameState -> Game Int
movePoints _ _ _ move Nothing = fail $ "Unexpected: invalid move generated: " ++ show move
movePoints i hand knownCards move (Just st) = do
    me <- getPlayer i
    currentSt <- get
    let newSz = newHandSize move hand
        bonus = if newSz == 0 then exitBonus else 0
    let myHandDivider = if oneCanExit st
                          then selfHandOneCanExitDivider
                          else selfHandNobodyCanExitDivider
    let currentPoints = myPoints myHandDivider i currentSt
    let newPoints = myPoints myHandDivider i st
        myNewHand = hands st !! i
    let delta = newPoints - currentPoints
    lift $ hFlush stdout
    t <- gets trash
    let newTrash = toTrash move : t
    let otherCards = M.elems knownCards
    ps <- gets players
    let otherPoints = [go player (C.fromList $ newTrash ++ hisHand) | (player, hisHand) <- zip ps otherCards]
    let maxOtherPoints = if newSz == 0
                           then 0
                           else if null otherPoints
                                  then 0
                                  else maximum otherPoints
    let myNewHand' = if Joker Black `C.elem` myNewHand
                       then if Joker Red `C.elem` myNewHand
                              then myNewHand
                              else C.insert (Joker Red) myNewHand
                       else C.insert (Joker Black) myNewHand
    let potentialPoints = go me myNewHand'
    let result = delta +
                 (potentialPoints `div` potentialMeldsDivider) -
                 (maxOtherPoints `div` otherPointsDivider) +
                 bonus
--     lift $ putStr $ printf "{δ %d; maxOtherPoints %d; potentialPoints %d}" delta maxOtherPoints potentialPoints
    return result
  where
    go p newHand =
      let list = possibleMelds p newHand
      in  if null list
            then 0
            else maximum $ map eval list
    eval meld = sum $ map meldPoints $ map snd $ meldCards' meld

    oneCanExit st = let ms i = [ [ c | (Player p, c) <- meldCards' meld, playerIdx p == i ] | meld <- melds st]
                        canExit i = any (>= 4) $ map length (ms i)
                        n = length (players st)
                    in  any canExit [0..n-1]

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
dropKnownCard p card st = st { aiKnownCards = M.update update p (aiKnownCards st) }
  where
    update list = let res = delete card list
                  in  if null res
                        then Nothing
                        else Just res

onPickTrash :: Int -> Player -> Int -> Game ()
onPickTrash me (Player p) n = do
  cards <- gets (take n . trash)
  modifyMe me $ addKnownCards (playerName p) cards

onTrash :: Int -> Player -> Card -> Game ()
onTrash me (Player p) card = do
  modifyMe me $ dropKnownCard (playerName p) card

onNewMeld me (Player p) cards = do
  forM_ cards $ \card -> do
    modifyMe me $ dropKnownCard (playerName p) card

onAddToMeld me (Player p) (card,_) = do
  modifyMe me $ dropKnownCard (playerName p) card

