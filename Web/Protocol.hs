{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, RecordWildCards, TypeFamilies #-}

module Web.Protocol where

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class 
import Control.Concurrent
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Data.Aeson hiding (Error)
import Data.Generics
import System.IO
import Text.Printf
import Text.Parsec (runParser)

import Cards
import Types
import Engine
import Parser (pCard)
import Web.WebSockets

data Message =
    Hello Text
  | Start
  | OK
  | Wait
  | Processing
  | Quit
  | Lock
  | Unlock
  | GiveCard Card
  | MoveMsg Int Move
  | MoveAction Int MoveAction
  | Error Text [MoveAction]
  deriving (Eq, Show, Typeable)

addPair k v (Object o) = Object $ H.insert k v o
addPair _ _ _ = error "Unexpected: addPair on not-an-object"

instance ToJSON Message where
  toJSON (Hello name) = object ["event" .= ("hello" :: Text), "name" .= name]
  toJSON Quit = object ["event" .= ("quit" :: Text)]
  toJSON OK = object ["event" .= ("ok" :: Text)]
  toJSON Start = object ["event" .= ("start" :: Text)]
  toJSON Wait = object ["event" .= ("wait" :: Text)]
  toJSON Processing = object ["event" .= ("processing" :: Text)]
  toJSON Lock = object ["event" .= ("lock" :: Text)]
  toJSON Unlock = object ["event" .= ("unlock" :: Text)]
  toJSON (GiveCard card) = object ["event" .= ("give" :: Text), "card" .= card]
  toJSON (MoveMsg i move) = addPair "event" "move" $ addPair "player" (toJSON i) $ toJSON move
  toJSON (MoveAction i a) = addPair "event" "action" $ addPair "player" (toJSON i) $ toJSON a
  toJSON (Error err actions) = object ["event" .= ("error" :: Text), "message" .= err, "actions" .= actions]

instance ToJSON CardColor where
  toJSON Red = String "red"
  toJSON Black = String "black"

instance ToJSON CardValue where
  toJSON v = String $ T.pack $ show v

instance ToJSON Suit where
  toJSON s = String $ T.pack $ showSuit s

instance ToJSON Card where
  toJSON (Joker Red)  = String "RJ"
  toJSON (Joker Black)  = String "BJ"
  toJSON (Card s v) = String $ T.pack $ show v ++ showSuit s

instance ToJSON MoveAction where
  toJSON (ChangeJoker color i) = object ["type" .= ("change" :: Text), "change" .= color, "meld" .= i]
  toJSON (PickTrash n) = object ["type" .= ("pick" :: Text), "n" .= n]
  toJSON (NewMeld _) = error $ "MoveAction.Meld.toJSON is not implemented"
  toJSON (MeldCard i card) = object ["type" .= ("meld" :: Text), "card" .= card, "meld" .= i]
  toJSON (AddToMeld card i) = object ["type" .= ("add" :: Text), "card" .= card, "meld" .= i]
  toJSON (Trash card) = object ["type" .= ("trash" :: Text), "card" .= card]

instance ToJSON Move where
  toJSON move = object $
      jsonChange (toChangeJoker move) ++
      jsonPick (toPickTrash move) ++
      jsonMelds (toNewMelds move) ++
      jsonAdd (toAddToMelds move) ++
      ["trash" .= toTrash move]
    where
      jsonChange Nothing = []
      jsonChange (Just (color,i)) = ["change" .= object ["color" .= color, "meld" .= i]]

      jsonPick Nothing = []
      jsonPick (Just n) = ["pick" .= n]

      jsonMelds [] = []
      jsonMelds melds = ["meld" .= melds]

      jsonAdd [] = []
      jsonAdd adds = ["add" .= adds]

instance ToJSON Meld where
  toJSON (Street {..}) =
    object ["id" .= meldId,
            "type" .= ("street" :: Text),
            "suit" .= streetSuit,
            "from" .= streetFrom,
            "to" .= streetTo,
            "owners" .= meldOwners,
            "jokers" .= meldJokers]
  toJSON (Avenue {..}) =
    object ["id" .= meldId,
            "type" .= ("avenue" :: Text),
            "value" .= avenueValue,
            "suits" .= avenueSuits,
            "owners" .= meldOwners,
            "jokers" .= meldJokers]

instance ToJSON Player where
  toJSON (Player p) = String $ T.pack $ playerName p

instance FromJSON Message where
  parseJSON x@(Object o) = do
    action <- o .: "event"
    case (action :: Text) of
      "hello" -> Hello <$> o .: "name"
      "ok" -> return OK
      "start" -> return Start
      "wait" -> return Wait
      "processing" -> return Processing
      "quit" -> return Quit
      "lock" -> return Lock
      "unlock" -> return Unlock
      "move" -> MoveMsg
                  <$> o .: "player"
                  <*> parseMove o
      "action" -> MoveAction
                    <$> o .: "player"
                    <*> parseAction o
      "give" -> parseGive o
      "error" -> Error
                   <$> o .: "message"
                   <*> o .: "actions"
  parseJSON x = fail $ "Invalid message object: " ++ show x

instance FromJSON MoveAction where
  parseJSON (Object o) = parseAction o
  parseJSON x = fail $ "Invalid object for move action: " ++ show x

parseChangeJoker o = do
   chgj <- o .:? "change"
   case chgj of
     Nothing -> return Nothing
     Just color -> do
                   i <- o .: "meld"
                   return $ Just (color, i)

parseMove o = do
  changeJoker <- parseChangeJoker o
  pick <- o .:? "pick"
  meldCards <- o .:? "melds" .!= []
  adds <- o .:? "add" .!= []
  trash <- o .: "trash"
  melds <- forM meldCards $ \cards ->
             case buildMeld undefined cards of
               Left err -> fail $ "Invalid meld: " ++ err
               Right meld -> return meld
  return $ Move changeJoker pick melds adds trash

instance FromJSON Card where
  parseJSON (String text) = do
    let str = T.unpack text
    case runParser pCard () str str of
      Right card -> return card
      Left err -> fail $ "Invalid card: " ++ str
  parseJSON x = fail $ "Invalid object for card: " ++ show x

parseGive o = GiveCard <$> (o .: "card")

parseAction o = do
  t <- o .: "type"
  case t of
    "change" -> do
                x <- parseChangeJoker o
                case x of
                  Nothing -> fail $ "No change joker info!"
                  Just (color,i) -> return $ ChangeJoker color i
    "pick" -> PickTrash <$> (o .: "n")
    "meld" -> parseMeld o
    "add" -> AddToMeld
               <$> o .: "card"
               <*> o .: "meld"
    "trash" -> Trash <$> (o .: "card")
    _ -> fail $ "Invalid action: " ++ T.unpack t

instance FromJSON CardColor where
  parseJSON (String "red") = return Red
  parseJSON (String "black") = return Black
  parseJSON x = fail $ "Invalid object for card color: " ++ show x

parseMeld o = MeldCard
               <$> o .: "meld"
               <*> o .: "card"

instance Protocol Message where
  type ProtocolState Message = (Chan Message, Chan Message)

  initProtocol _ = return ()

  onClientMessage sink msg (fromPlayer, toPlayer) = do
    putStrLn $ "onClientMessage: " ++ show msg
    writeChan toPlayer msg
    res <- readChan fromPlayer
    putStrLn $ "Server answer: " ++ show res
    sendMessage sink res

  getHelloUsername (Hello name) = Just name
  getHelloUsername _ = Nothing

  isQuit Quit = True
  isQuit _ = False
  
