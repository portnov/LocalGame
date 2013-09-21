{-# LANGUAGE TypeFamilies, DeriveDataTypeable, RecordWildCards #-}

module AI where

import Control.Monad.IO.Class
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
import qualified AI.Config as Config

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
    aiKnownCards :: M.Map String [Card],
    aiConfig :: Config.Config }
  deriving (Eq, Typeable)

instance Show AI where
  show (AI i _ _) = "AI #" ++ show i

data TerminatingCondition = TC {
    tcMaxMoves :: Int,
    tcMaxPoints :: Int,
    tcMinPoints :: Int,
    tcMovesEdge :: Int }
  deriving (Eq, Show)

defaultTC :: TerminatingCondition
defaultTC = TC 2000 90 10 1000

data MoveResult = MoveResult {
    mrCurrentPoints :: Double,
    mrOneCanExit :: Bool,
    mrMyMelds :: Double,
    mrMyHand :: Double,
    mrMyHandSize :: Double,
    mrOtherPoints :: Double,
    mrPotentialPoints :: Double,
    mrBonus :: Double }
  deriving (Eq)

instance Show MoveResult where
  show (MoveResult {..}) =
    printf "{current %0.1f; my melds %0.1f; my hand %0.1f; other points %0.1f; potential points %0.1f; bonus %0.0f}"
           mrCurrentPoints mrMyMelds mrMyHand mrOtherPoints mrPotentialPoints mrBonus

instance IsPlayer AI where
  playerName ai = show ai
  playerIdx (AI i _ _) = i

  initPlayer me@(AI i _ _) = do
    config <- liftIO $ Config.load i
    ps <- gets players
    let knownCards = M.fromList [(playerName p, []) | Player p <- ps, playerIdx p /= i]
    setPlayer i $ Player $ me {aiConfig = config, aiKnownCards = knownCards}

  onEndGame me@(AI i _ config) _ = do
    currPoints <- getPoints i
    let avg = (fromIntegral currPoints + fromIntegral (Config.nGames config) * Config.avgPoints config) / fromIntegral (Config.nGames config + 1)
    let newConfig = config {
                     Config.avgPoints = avg,
                     Config.nGames = Config.nGames config + 1 }
    liftIO $ Config.save i newConfig
    liftIO $ putStrLn $ printf "AI#%d avg points: %0.2f" i avg

  playerSelectMove me@(AI i knownCards config) = do
      hand <- getHand i
      let totalKnownCards = sum $ map length $ M.elems knownCards
      liftIO $ putStrLn $ printf "AI#%d has %d cards in its hand. It knows %d cards of other players." i (C.size hand) totalKnownCards 
      liftIO $ putStr $ "Generating list of possible moves: "
      liftIO $ hFlush stdout
      moves <- validMoves (Player me) hand
      let nMoves = length moves
      if null moves
        then if C.size hand == 1
               then do
                    liftIO $ putStrLn $ " single card, put it to trash and exit."
                    return $ Move {
                              toChangeJoker = Nothing,
                              toPickTrash = Nothing,
                              toNewMelds = [],
                              toAddToMelds = [],
                              toTrash = head (C.toList hand) }
               else fail "Unexpected: no moves."
        else do
              rs <- mapM go moves
              liftIO $ putStr $ printf " done.\nSelecting best of %d moves: " nMoves
              liftIO $ hFlush stdout
              currentSt <- get
              let currentPoints = myPoints 1 i currentSt
                  tc = if currentPoints >= 0
                         then defaultTC
                         else defaultTC {tcMinPoints = -currentPoints}
              deckLeft <- gets (length . deck)
              nPlayers <- gets (length . players)
              let section = if deckLeft <= 4 * nPlayers
                              then Config.onEndspiel config
                              else if deckLeft <= (54 - 7 * nPlayers)
                                     then Config.onMittelspiel config
                                     else Config.onDebut config
              liftIO $ putStr $ "using config section: " ++ show section
              liftIO $ hFlush stdout
              resultMs <- iter tc (points section hand) $ zip3 [0..] moves rs
              let results = map snd resultMs
                  maxPoints = maximum results
                  Just moveIdx = findIndex (== maxPoints) results
                  move = fst $ resultMs !! moveIdx
                  seenMoves = length resultMs
              liftIO $ putStrLn $ printf "\nAI#%d selected move #%d (points %0.1f): %s" i (seenMoves - moveIdx - 1) maxPoints (show move)
              return move
    where
      go move = do
        r <- evalGame $ evalMove i move
        return r

      points section hand j move r = do
        r <- movePoints i hand knownCards move r
        liftIO $ putStr $ show r
        liftIO $ hFlush stdout
        let p = evalMovePoints section r
        liftIO $ putStr $ printf "[%d:%0.1f]" (j :: Int) p
        liftIO $ hFlush stdout
        return p

      iter :: TerminatingCondition
           -> (Int -> Move -> Maybe GameState -> Game Double)
           -> [(Int, Move, Maybe GameState)] -> Game [(Move, Double)]
      iter tc fn ys = iterGo [] tc fn ys

      checkTC tc p j = do
        if round p >= tcMaxPoints tc
          then return True
          else if j >= tcMovesEdge tc && round p >= tcMinPoints tc
                 then return True
                 else if j >= tcMaxMoves tc
                        then return True
                        else return False

      iterGo :: [(Move,Double)] -> TerminatingCondition
             -> (Int -> Move -> Maybe GameState -> Game Double)
             -> [(Int, Move, Maybe GameState)] -> Game [(Move, Double)]
      iterGo acc _ _ [] = return acc
      iterGo acc tc fn ((j, move, r):xs) = do
        p <- fn j move r
        end <- checkTC tc p j
        if end
          then return ((move,p):acc)
          else iterGo ((move,p):acc) tc fn xs
                 
  beforeMove me@(AI i _ _) player@(Player p) move = do
    if playerName p == playerName me
      then return ()
      else do
           whenJust (toPickTrash move) (onPickTrash i player)
           forM_ (toNewMelds move) $ \meld -> onNewMeld i player $ map snd $ meldCards meld
           forM_ (toAddToMelds move) (onAddToMeld i player)
           onTrash i player (toTrash move)

movePoints :: Int -> Hand -> M.Map String [Card] -> Move -> Maybe GameState -> Game MoveResult
movePoints _ _ _ move Nothing = fail $ "Unexpected: invalid move generated: " ++ show move
movePoints i hand knownCards move (Just st) = do
    me <- getPlayer i
    currentSt <- get
    let newSz = newHandSize move hand
        bonus = if newSz == 0 then exitBonus else 0
    let myNewHand = hands st !! i
        newMeldPoints = myMelds i st
        newHandPoints = myHandPoints i st
    let currentPoints = myPoints 1 i currentSt
    liftIO $ hFlush stdout
    t <- gets trash
    let newTrash = toTrash move : t
    let otherCards = M.elems knownCards
    let addJoker hand = if Joker Black `C.elem` hand
                         then if Joker Red `C.elem` hand
                                then hand
                                else C.insert (Joker Red) hand
                         else C.insert (Joker Black) hand
    ps <- gets players
    let otherPlayers = [actor | actor@(Player p) <- ps, playerIdx p /= i]
    let otherPoints = [go player (addJoker $ C.fromList $ newTrash ++ hisHand) | (player, hisHand) <- zip ps otherCards]
    liftIO $ putStr $ show otherPoints
    let maxOtherPoints = if newSz == 0
                           then 0
                           else if null otherPoints
                                  then 0
                                  else maximum otherPoints

    let myNewHand' = addJoker myNewHand
    let potentialPoints = go me myNewHand'
    return $ MoveResult {
               mrCurrentPoints = fromIntegral currentPoints,
               mrOneCanExit = oneCanExit st,
               mrMyMelds = fromIntegral newMeldPoints,
               mrMyHand = fromIntegral newHandPoints,
               mrMyHandSize = fromIntegral (C.size myNewHand),
               mrOtherPoints = fromIntegral maxOtherPoints,
               mrPotentialPoints = fromIntegral potentialPoints,
               mrBonus = fromIntegral bonus }
  where
    go p newHand =
      let list = possibleMelds p newHand
          ms = map fst $ possibleAddToMelds (melds st) newHand
          addPoints = sum $ map meldPoints ms
      in  if null list
            then addPoints
            else sum $ addPoints : map eval list

    eval meld = sum $ map meldPoints $ map snd $ meldCards' meld

    oneCanExit st = let ms i = [ [ c | (Player p, c) <- meldCards' meld, playerIdx p == i ] | meld <- melds st]
                        canExit i = any (>= 4) $ map length (ms i)
                        n = length (players st)
                    in  any canExit [0..n-1]

evalMovePoints :: Config.Section -> MoveResult -> Double
evalMovePoints (Config.Section {..}) (MoveResult {..}) =
  let handCoef = if mrOneCanExit
                   then myHandOneCanExitCoef
                   else myHandNobodyCanExitCoef
      handSizeCoef = if mrOneCanExit
                       then myHandSizeOneCanExitCoef
                       else myHandSizeNobodyCanExitCoef
      delta = mrMyMelds - (handCoef * mrMyHand) - mrCurrentPoints
  in  delta - (otherPointsCoef * mrOtherPoints) +
      (potentialPointsCoef * mrPotentialPoints) +
      (handSizeCoef * mrMyHandSize) +
      (bonusCoef * mrBonus)

modifyMe :: Int -> (AI -> AI) -> Game ()
modifyMe i fn = do
  Player p <- getPlayer i
  case cast p of
    Just me@(AI {}) -> setPlayer i $ Player (fn me)
    Nothing -> fail $ printf "Player #%d is not AI!" i

ai :: Int -> Player
ai i = Player (AI i M.empty Config.defaultConfig)

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

onNewMeld me (Player p) cards = do
  forM_ cards $ \card -> do
    modifyMe me $ dropKnownCard (playerName p) card

onAddToMeld me (Player p) (card,_) = do
  modifyMe me $ dropKnownCard (playerName p) card

