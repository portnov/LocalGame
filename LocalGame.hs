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
import qualified CardSet as C
import Types
import Parser
import Engine
import AI
import qualified Web.Client as W

readMove :: Player -> String -> Game Move
readMove p str =
  case runParser (moveActions p) () "<input>" str of
    Right res -> buildMove p res
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

  onGiveCard (Human i) card = do
    lift $ putStrLn $ printf "H#%d new card: %s" i (show card)

  playerSelectMove u@(Human i) = do
    hand <- getHand i
    lift $ putStrLn $ show u ++ " hand: " ++ show hand
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
    let ns = [0 .. length ps - 1]
    go (cycle ns)
  where
    go [] = return ()
    go (i:ns) = do
      giveCard i
      hs <- gets hands
      if any C.null hs
        then endOfGame
        else do
          actor <- getPlayer i
          case actor of
            Player player -> do
              st <- get
              lift $ putStrLn $ "Trash: " ++ unwords (map show $ trash st)
              lift $ putStrLn $ "Melds:\n" ++ unwords (map showMeld (melds st))
              lift $ putStrLn $ printf "Player %s move:" (playerName player)
              runPlayer i actor
              go ns

    endOfGame = do
       st <- get
       let ps = players st
           ns = [0 .. length ps - 1]
       forM_ (players st) $ \(Player player) -> do
          onEndGame player
       lift $ putStrLn "End of game. States:"
       lift $ print st
       forM_ ns $ \i -> do
         points <- getPoints i
         lift $ putStrLn $ printf "Player #%d points: %d" i points

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
              valid <- isMoveValid' actor move
              when (not valid) $
                  fail $ "Move is not valid."
              allOnMove actor move
              evalMove i move
      if ok
        then return ()
        else go (n-1)

makePlayer :: String -> Int -> Player
makePlayer "ai" i = ai i
makePlayer "human" i = human i
makePlayer "web" i = W.webPlayer i
makePlayer str _ = error $ "Unknown player type: " ++ str

main = do
  argv <- getArgs
  let playerTypes = if null argv
                      then ["ai", "ai"]
                      else argv
  let players = zipWith makePlayer playerTypes [0..]
  runStateT (testGame $ length players) (emptyState players)

