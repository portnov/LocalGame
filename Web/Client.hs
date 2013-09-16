{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

module Web.Client where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Exception
import Control.Monad.IO.Class 
import Control.Concurrent
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson hiding (Error)
import Data.Generics
import System.IO
import Text.Printf
import Text.Parsec (runParser)

import Cards
import Types
import Engine
import qualified CardSet as C
import Parser (pCard)
import Web.WebSockets
import Web.Protocol

data WebPlayer = WebPlayer {
       userNumber :: Int,
       chanPush :: Chan (Destination,Message),
       chanFromClient :: Chan Message,
       chanToClient :: Chan Message }
  deriving (Eq, Typeable)

webPlayer :: Int -> Player
webPlayer i = Player $ WebPlayer i undefined undefined undefined

just me = Username (T.pack $ playerName me)

instance Show WebPlayer where
  show (WebPlayer i _ _ _) = printf "W#%d" i

instance IsPlayer WebPlayer where
  playerName u = show u
  playerIdx (WebPlayer i _ _ _) = i

  beforeGiveCards me = do
    push <- liftIO $ newChan
    from <- liftIO $ newChan
    to <- liftIO $ newChan
    setPlayer (playerIdx me) $ Player $ me {chanPush = push, chanFromClient = from, chanToClient = to}
    liftIO $ forkIO $ runWS defaultWSConfig push (to,from)
    liftIO $ waitStart from to
    return ()

  onInitialTrash me card = do
    liftIO $ writeChan (chanPush me) (just me, MoveAction 0 $ Trash card)

  onGiveCard me card = do
    liftIO $ writeChan (chanPush me) (just me, GiveCard card)

  onEndGame me (Player winner) = do
    ps <- gets players
    let ns = [0 .. length ps - 1]
    scores <- forM ns $ getPoints
    liftIO $ writeChan (chanPush me) (just me, Scores (T.pack $ playerName winner) scores)

  afterMove me (Player player) move = do
    when (playerIdx me /= playerIdx player) $ do
       let dst = just me
       liftIO $ writeChan (chanPush me) (dst, Lock)
       st <- getClientState (playerIdx me)
       liftIO $ writeChan (chanPush me) (dst, SetState st)

  playerSelectMove me@(WebPlayer _ push fromClient toClient) = do
    liftIO $ writeChan push (just me, Unlock)
    move <- readMove me fromClient toClient
    return move

onPickTrash i n = MoveAction i $ PickTrash n

onNewMeld i meldId card = MoveAction i $ MeldCard meldId card

onAddToMeld i (card, meldId) = MoveAction i $ AddToMeld card meldId

onTrash i card = MoveAction i $ Trash card

onChangeJoker i color meldId card = MoveAction i $ ChangeJoker color meldId (Just card)

waitStart fromClient toClient = do
  msg <- readChan fromClient
  case msg of
    Start -> do
             putStrLn "Start game"
             writeChan toClient OK
             return ()
    _ -> do
         putStrLn $ "Unexpected message from client before Hello: " ++ show msg
         waitStart fromClient toClient

readMove me fromClient toClient = do
  liftIO $ putStrLn "Waiting move from web client..."
  actions <- liftIO $ readMoveActions [] fromClient toClient
  case buildMove (Player me) actions of
    Left err -> do
      st <- getClientState (playerIdx me)
      liftIO $ writeChan toClient $ Error (T.pack $ "Incomplete move: " ++ err) (Just st)
      readMove me fromClient toClient
    Right move -> do
      r <- checkMoveM (Player me) move
      case r of
        Nothing -> do
                   liftIO $ writeChan toClient Lock
                   return move
        Just err -> do
                    st <- getClientState (playerIdx me)
                    liftIO $ writeChan toClient $ Error (T.pack $ "Forbidden move: " ++ err) (Just st)
                    readMove me fromClient toClient

getClientState i = do
    t <- gets (reverse . trash)
    ms <- gets melds
    hand <- getHand i
    let ms' = map convert ms
    return $ ClientState t ms' (C.toList hand)
  where
    convert meld = [(T.pack (playerName p), card) | (Player p, card) <- meldCards meld]

readMoveActions acc fromClient toClient = do
  msg <- readChan fromClient
  putStrLn $ "readMoveActions: " ++ show msg
  case validateMsg msg of
    NoAction msg -> do
                    writeChan toClient msg
                    readMoveActions acc fromClient toClient
    Action _ action -> do
      writeChan toClient Wait
      readMoveActions (action:acc) fromClient toClient
    Finish -> do
      putStrLn $ "Finish. Actions: " ++ show acc
      return acc

data ValidateState = NoAction Message | Action Int MoveAction | Finish
  deriving (Eq, Show)

validateMsg (Error err xs) = NoAction $ Error ("Client-generated error message: " `T.append` err) xs
validateMsg (MoveAction i action) = Action i action
validateMsg OK = Finish
validateMsg msg = NoAction $ Error (T.pack $ "Message from client is not supported: " ++ show msg) Nothing


      
