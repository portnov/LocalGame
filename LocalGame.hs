{-# LANGUAGE ExistentialQuantification, TypeFamilies, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Error
import Control.Exception
import Data.Generics
import Data.List
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
                liftIO $ putStrLn $ "Error: " ++ show err
                liftIO $ putStr $ show p ++ " move: "
                liftIO $ hFlush stdout
                str <- liftIO $ getLine
                readMove p str

data Human = Human { userNumber :: Int }
  deriving (Eq, Typeable)

instance Show Human where
  show (Human i) = printf "H#%d" i

instance IsPlayer Human where
  playerName u = show u
  playerIdx (Human i) = i

  onGiveCard (Human i) card = do
    liftIO $ putStrLn $ printf "H#%d new card: %s" i (show card)

  playerSelectMove u@(Human i) = do
    hand <- getHand i
    liftIO $ putStrLn $ show u ++ " hand: " ++ show hand
    liftIO $ putStr $ show u ++ " move: "
    liftIO $ hFlush stdout
    str <- liftIO $ getLine
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
              liftIO $ putStrLn $ "Trash: " ++ unwords (map show $ trash st)
              liftIO $ putStrLn $ "Melds:\n" ++ unwords (map showMeld (melds st))
              liftIO $ putStrLn $ printf "Player %s move:" (playerName player)
              runPlayer i actor
              go ns

    endOfGame = do
       st <- get
       let ps = players st
           ns = [0 .. length ps - 1]
       hs <- gets hands
       i <- case findIndex C.null hs of
              Nothing -> fail $ "Impossible: no empty hands on end of game"
              Just i -> return i
       winner <- getPlayer i
       forM_ (players st) $ \(Player player) -> do
          onEndGame player winner
       liftIO $ putStrLn "End of game. States:"
       liftIO $ print st
       forM_ ns $ \i -> do
         points <- getPoints i
         liftIO $ putStrLn $ printf "Player #%d points: %d" i points

allBeforeMove actor@(Player player) move = do
  ps <- gets players
  forM_ ps $ \(Player p) -> do
    beforeMove p actor move

allAfterMove actor@(Player player) move = do
  ps <- gets players
  forM_ ps $ \(Player p) -> do
    afterMove p actor move
    
runPlayer i actor@(Player player) = go 3
  where
    go 0 = fail "Too many errors, exiting."
    go n = do
      ok <- atomicallyTry True $ do
              move <- playerSelectMove player
              valid <- isMoveValid' actor move
              when (not valid) $
                  fail $ "Move is not valid."
              allBeforeMove actor move
              evalMove i move
              allAfterMove actor move
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
  runStateT (runErrorT $ testGame $ length players) (emptyState players)

