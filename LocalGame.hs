{-# LANGUAGE ExistentialQuantification, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
import Control.Monad
import Control.Monad.State
import Control.Exception
import Text.Printf
import Text.Parsec (runParser)
import System.IO

import Cards
import Types
import Parser
import Engine

instance Exception String

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
  deriving (Eq)

instance Show Human where
  show (Human i) = printf "Human #%d" i

instance IsPlayer Human where
  type PlayerState Human = ()

  playerName u = show u
  playerSelectMove u@(Human i) = do
    hand <- getHand i
    lift $ putStrLn $ show u ++ " hand: " ++ unwords (map show hand)
    lift $ putStr $ show u ++ " move: "
    lift $ hFlush stdout
    str <- lift $ getLine
    readMove (Player u ()) str

human :: Int -> Player
human i = Player (Human i) ()

testGame :: Game ()
testGame = do
  initGame 2 3
  runGame

runGame :: Game ()
runGame = do
  ps <- gets players
  let ns = [0.. length ps - 1]
  forM_ ns $ \i -> do
    giveCard i
    let player = ps !! i
    st <- get
    lift $ print st
    lift $ putStrLn $ printf "Player %s move:" (playerName player)
    runPlayer i player
  hs <- gets hands
  if any null hs
    then do
         st <- get
         lift $ putStrLn "End of game. States:"
         lift $ print st
    else runGame

runPlayer i player = do
  ok <- atomicallyTry $ do
          move <- playerSelectMove player
          evalMove i move
  if ok
    then return ()
    else runPlayer i player

main = do
  let players = [human 0, human 1]
  runStateT testGame (emptyState players)

