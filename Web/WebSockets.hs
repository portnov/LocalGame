{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Web.WebSockets where

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class 
import Control.Concurrent
import qualified Network.Socket as S
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Char
import Data.Aeson
import System.IO

type Sink = WS.Sink WS.Hybi00
type Clients = M.Map Text Sink

data WSConfig = WSConfig {
  wscHost :: String,
  wscPort :: Int }
  deriving (Eq, Show)

defaultWSConfig :: WSConfig
defaultWSConfig = WSConfig "0.0.0.0" 9160

instance FromJSON WSConfig where
  parseJSON (Object o) =
    WSConfig
      <$> o .:? "host" .!= "0.0.0.0"
      <*> o .:? "port" .!= 9160
  parseJSON x = fail $ "Invalid object for WS config: " ++ show x

instance ToJSON WSConfig where
  toJSON wsc = object ["host" .= wscHost wsc, "port" .= wscPort wsc]

class (FromJSON message, ToJSON message, Show message) => Protocol message where
  onClientMessage :: Sink -> message -> IO ()
  getHelloUsername :: message -> Maybe Text
  isQuit :: message -> Bool

runWS :: (Protocol message) => WSConfig -> Chan message -> IO ()
runWS cfg chan = do
  var <- newMVar M.empty
  forkIO $ sendEvents var chan
  runServer (wscHost cfg) (wscPort cfg) $ application var chan

-- | Provides a simple server.
runServer :: WS.Protocol p
          => String                        -- ^ Address to bind to
          -> Int                           -- ^ Port to listen on
          -> (WS.Request -> WS.WebSockets p ())  -- ^ Application to serve
          -> IO ()                         -- ^ Never returns
runServer host port ws = S.withSocketsDo $ do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    _ <- S.setSocketOption sock S.ReuseAddr 1
    host' <- S.inet_addr host
    S.bindSocket sock (S.SockAddrInet (fromIntegral port) host')
    putStrLn "Listening..."
    S.listen sock 5
    handle (closeSock sock) $ forever ( do
        putStrLn "Accepting..."
        (conn, _) <- S.accept sock
        putStrLn "Connection accepted"
        _ <- forkIO $ do
                WS.runWithSocket conn ws
                putStrLn "app run."
        return () )
  where
    closeSock :: S.Socket -> SomeException -> IO ()
    closeSock sock e = do
      print e
      S.sClose sock

application :: forall message. Protocol message => MVar Clients -> Chan message -> WS.Request -> WS.WebSockets WS.Hybi00 ()
application var chan rq = do
    WS.acceptRequest rq
    liftIO $ print rq
    text <- WS.receiveData
    msg <- parseText text
    sink <- WS.getSink
    case getHelloUsername (msg :: message) of
      Just name -> do
        liftIO $ putStrLn $ "Client sent Hello."
        talk var name sink
      Nothing ->
        if isQuit msg
          then liftIO $ putStrLn "Client send Quit, quiting."
          else liftIO $ onClientMessage sink msg
  where
    talk var name sink = do
      liftIO $ addClient var name sink
      flip WS.catchWsError catchDisconnect $  forever $ do
        text <- WS.receiveData
        msg <- parseText text
        liftIO $ onClientMessage sink (msg :: message)
      where
        catchDisconnect e = case fromException e of
            Just WS.ConnectionClosed -> liftIO $ removeClient var name
            _ -> return ()

addClient var name sink = do
  putStrLn $ "New client: " ++ T.unpack name
  modifyMVar_ var $ \st ->
    return $ M.insert name sink st

removeClient var name = do
  putStrLn $ "Client quit: " ++ T.unpack name
  modifyMVar_ var $ \st ->
    return $ M.delete name st

sendMessage sink msg =
  WS.sendSink sink $ WS.textData $ encodeMsg msg

sendEvents var chan = do
  msg <- readChan chan
  putStrLn $ "Server message: " ++ show msg
  clients <- readMVar var
  forM_ (M.assocs clients) $ \(name, sink) -> do
    putStrLn $ "Sending message to " ++ T.unpack name
    if isQuit msg
      then do
           putStrLn "Quiting."
           return ()
      else do 
           sendMessage sink msg
           sendEvents var chan

parseStr :: (Monad m, FromJSON message) => String -> m message
parseStr str = do
  let bstr = L.pack $ map (fromIntegral . ord) str
  case eitherDecode bstr of
    Left err -> fail err
    Right res -> return res

parseText :: (MonadIO m, FromJSON message) => Text -> m message
parseText text = do
  let str = T.unpack text
  liftIO $ putStrLn $ "Client msg: " ++ str
  let bstr = L.pack $ map (fromIntegral . ord) str
  case eitherDecode bstr of
    Left err -> fail err
    Right res -> return res

encodeMsg :: ToJSON message => message -> Text
encodeMsg msg =
  let bstr = encode msg
  in  T.pack $ map (chr . fromIntegral) $ L.unpack bstr

