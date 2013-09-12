{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

import Control.Monad
import Control.Exception
import Control.Monad.IO.Class 
import Control.Concurrent
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson
import System.IO

import Web.WebSockets

data Message =
    Hello Text
  | Quit
  | OK
  | Event Text
  deriving (Eq, Show)

instance ToJSON Message where
  toJSON (Hello name) = object ["action" .= ("hello" :: Text), "user" .= name]
  toJSON Quit = object ["action" .= ("quit" :: Text)]
  toJSON OK = object ["action" .= ("ok"  :: Text)]
  toJSON (Event text) = object ["action" .= ("event" :: Text), "text" .= text]

instance FromJSON Message where
  parseJSON (Object o) = do
    action <- o .: "action"
    case action of
      "quit" -> return Quit
      "ok" -> return OK
      "event" -> do
                 text <- o .: "text"
                 return $ Event text
      "hello" -> do
                 name <- o .: "name"
                 return $ Hello name
      _ -> fail $ "Unknown action: " ++ T.unpack action
  parseJSON x = fail $ "Invalid object for event: " ++ show x

instance Protocol Message where
  type ProtocolState Message = ()

  initProtocol _ = return ()

  onClientMessage sink msg _ = do
    putStrLn $ "Message from client: " ++ show msg
    sendMessage sink OK

  getHelloUsername msg =
    case msg of
      Hello name -> Just name
      _ -> Nothing

  isQuit Quit = True
  isQuit _ = False


main :: IO ()
main = do
  chan <- newChan
  forkIO $ input chan
  runWS defaultWSConfig chan ()

input chan = do
  putStr "Input: "
  hFlush stdout
  str <- getLine
  let msg = if str == "quit"
              then Quit
              else Event (T.pack str)
  writeChan chan msg
  if msg == Quit
    then return ()
    else input chan


