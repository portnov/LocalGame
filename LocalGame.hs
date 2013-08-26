{-# LANGUAGE ExistentialQuantification, TypeFamilies, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
import Control.Monad
import Control.Monad.State
import Control.Exception
import Data.Generics
import Text.Printf
import Text.Parsec (runParser)
import System.IO
import System.Environment

import Cards
import Types
import Parser
import Engine
import AI

readMove :: Player -> String -> Game Move
readMove p str =
  case runParser (moveActions p) () "<input>" str of
    Right res -> buildMove res
    Left err -> do
                lift $ putStrLn $ "Error: " ++ show err
                lift $ putStr $ show p ++ " move: "
                lift $ hFlush stdout
                str <- lift $ getLine
                readMove p str

data Human = Human { userNumber :: Int }
  deriving (Eq, Typeable)

instance Show Human where
  show (Human i) = printf "H#%d" i

instance IsPlayer Human where
  playerName u = show u
  playerIdx (Human i) = i
  playerSelectMove u@(Human i) = do
    hand <- getHand i
    lift $ putStrLn $ show u ++ " hand: " ++ unwords (map show hand)
    lift $ putStr $ show u ++ " move: "
    lift $ hFlush stdout
    str <- lift $ getLine
    readMove (Player u) str

human :: Int -> Player
human i = Player (Human i)

testGame :: Int -> Game ()
testGame nPlayers = do
  initGame nPlayers 3
  runGame

runGame :: Game ()
runGame = do
  ps <- gets players
  let ns = [0.. length ps - 1]
  forM_ ns $ \i -> do
    giveCard i
    case ps !! i of
      p@(Player player) -> do
        st <- get
        lift $ putStrLn $ "Trash: " ++ unwords (map show $ trash st)
        lift $ putStrLn $ "Melds:\n" ++ unwords (map showMeld (melds st))
        lift $ putStrLn $ printf "Player %s move:" (playerName player)
        runPlayer i p
  hs <- gets hands
  if any null hs
    then do
         st <- get
         lift $ putStrLn "End of game. States:"
         lift $ print st
         forM_ ns $ \i -> do
           points <- getPoints i
           lift $ putStrLn $ printf "Player #%d points: %d" i points
    else runGame

allOnMove actor@(Player player) move = do
  ps <- gets players
  forM_ ps $ \(Player p) -> do
    onMove p actor move
    

runPlayer i actor@(Player player) = go 3
  where
    go 0 = fail "Too many errors, exiting."
    go n = do
      ok <- atomicallyTry True $ do
              move <- playerSelectMove player
              allOnMove actor move
              evalMove i move
      if ok
        then return ()
        else go (n-1)

makePlayer :: String -> Int -> Player
makePlayer "ai" i = ai i
makePlayer "human" i = human i
makePlayer str _ = error $ "Unknown player type: " ++ str

main = do
  argv <- getArgs
  let playerTypes = if null argv
                      then ["ai", "ai"]
                      else argv
  let players = zipWith makePlayer playerTypes [0..]
  runStateT (testGame $ length players) (emptyState players)

