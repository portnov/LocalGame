{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

module Web.Client where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Exception
import Control.Monad.IO.Class 
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Data.Aeson hiding (Error)
import Data.Generics
import Data.Monoid
import System.IO
import System.FilePath
import System.FilePath.Glob
import Text.Printf
import Text.Parsec (runParser)
import Data.Text.Format
import qualified Network.WebSockets as WS
import Data.IORef
import qualified Data.Text.Format.Params as Params
import Text.Localize hiding (__, lprintf)
import qualified Text.Localize as Localize

import Cards
import Types
import Engine
import qualified CardSet as C
import Parser (pCard)
import Web.WebSockets
import Web.Protocol

__ :: TL.Text -> LocalizedString
__ text = Localize.__ text

lprintf :: Params.Params ps => TL.Text -> ps -> LocalizedString
lprintf = Localize.lprintf

data WebPlayer = WebPlayer {
       userNumber :: Int,
       chanPush :: Chan (Destination, Message),
       chanFromClient :: Chan Message,
       chanToClient :: Chan Message }
  deriving (Typeable)

instance Eq WebPlayer where
  w1 == w2 = userNumber w1 == userNumber w2

webPlayer :: Int -> Player
webPlayer i = Player $ WebPlayer i undefined undefined undefined

just me = Username (T.pack $ playerName me)

instance Show WebPlayer where
  show (WebPlayer i _ _ _) = printf "W#%d" i

enumLanguages :: IO [(LanguageId, FilePath)]
enumLanguages = do
  files <- glob "mo/*.mo"
  let langs = map (\f -> take (length f - 3) f) $ map takeFileName files
  return $ zip langs files

instance IsPlayer WebPlayer where
  playerName u = show u
  playerIdx (WebPlayer i _ _ _) = i

  beforeGiveCards me = do
    push <- liftIO $ newChan
    from <- liftIO $ newChan
    to <- liftIO $ newChan
    setPlayer (playerIdx me) $ Player $ me {chanPush = push, chanFromClient = from, chanToClient = to}
    pairs <- liftIO $ enumLanguages
    trans <- liftIO $ loadTranslations pairs
    langVar <- liftIO $ newIORef "C"
    liftIO $ forkIO $ runWS defaultWSConfig push (trans, langVar, to,from)
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
       let msg = describeMove move
       liftIO $ writeChan (chanPush me) (dst, MoveMsg (playerIdx player) msg)
       liftIO $ writeChan (chanPush me) (dst, Lock)
       st <- getClientState (playerIdx me)
       liftIO $ writeChan (chanPush me) (dst, SetState st)

  playerSelectMove me@(WebPlayer _ push fromClient toClient ) = do
    liftIO $ writeChan push (just me, Unlock)
    move <- readMove me fromClient toClient push
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

readMove me fromClient toClient push = do
  liftIO $ putStrLn "Waiting move from web client..."
  st <- getClientState (playerIdx me)
  actions <- readMoveActions (just me) st [] fromClient toClient push
  case buildMove (Player me) actions of
    Left err -> do
      st <- getClientState (playerIdx me)
      liftIO $ writeChan toClient $ Error (__ "Incomplete move: " <> err) (Just st)
      readMove me fromClient toClient push
    Right move -> do
      r <- checkMoveM (Player me) move
      case r of
        Nothing -> do
                   liftIO $ writeChan toClient Lock
                   return move
        Just err -> do
                    st <- getClientState (playerIdx me)
                    liftIO $ writeChan toClient $ Error (__ "Forbidden move: " <> err) (Just st)
                    readMove me fromClient toClient push

getClientState i = do
    t <- gets (reverse . trash)
    ms <- gets melds
    hand <- getHand i
    let ms' = map convert ms
    return $ ClientState t ms' (C.toList hand)
  where
    convert meld = [(T.pack (playerName p), card) | (Player p, card) <- meldCards meld]

readMoveActions dst st acc fromClient toClient push = do
  msg <- liftIO $ readChan fromClient
  liftIO $ putStrLn $ "readMoveActions: " ++ show msg
  res <- validateMsg st msg
  case res of
    NoAction msg -> do
                    liftIO $ writeChan toClient msg
                    readMoveActions dst st acc fromClient toClient push
    Action _ action mmsg -> do
      liftIO $ writeChan toClient Wait
      whenJust mmsg $ \msg ->
        liftIO $ writeChan push (dst,msg)
      readMoveActions dst st (action:acc) fromClient toClient push
    Clear st -> do
      liftIO $ writeChan toClient $ SetState st
      readMoveActions dst st [] fromClient toClient push
    Finish -> do
      liftIO $ putStrLn $ "Finish. Actions: " ++ show acc
      return acc

data ValidateState = NoAction Message
                   | Action Int MoveAction (Maybe Message)
                   | Finish
                   | Clear ClientState
  deriving (Eq, Show)

validateMsg _ (Error err xs) = return $ NoAction $ Error (__ "Client-generated error message: " <> err) xs
validateMsg st (MoveAction i (ChangeJoker color meldId Nothing)) = do
    res <- getJokerValue meldId color
    case res of
      Nothing -> return $ NoAction $ Error (lprintf "No {} joker in meld #{}" (show color, meldId)) Nothing
      Just card -> do
                   hand <- getHand i
                   if card `C.elem` hand
                     then do
                          let action = ChangeJoker color meldId (Just card)
                              msg = MoveAction i action
                          return $ Action i action (Just msg)
                     else return $ NoAction $ Error (lprintf "No {} in your hand" (Only $ show card)) (Just st)
validateMsg _ (MoveAction i action) = return $ Action i action Nothing
validateMsg _ OK = return Finish
validateMsg st Cancel = return $ Clear st
validateMsg _ msg = return $ NoAction $ Error (lprintf "Message from client is not supported: {}" (Only $ show msg)) Nothing


      
