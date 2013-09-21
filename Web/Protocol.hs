{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, RecordWildCards, TypeFamilies #-}

module Web.Protocol where

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class 
import Control.Concurrent
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import Data.Char (chr, ord)
import Data.Generics
import System.IO
import Text.Printf
import Text.Parsec (runParser)
import Text.Localize
import qualified Network.WebSockets as WS

import Cards
import Types
import Engine
import Parser (pCard)
import Web.WebSockets

data Message =
    Hello Text
  | Start
  | OK
  | Cancel
  | Wait
  | Processing
  | Quit
  | Lock
  | Unlock
  | Scores Text [Int]
  | GiveCard Card
  | MoveMsg Int LocalizedString
  | MoveAction Int MoveAction
  | Error LocalizedString (Maybe ClientState)
  | SetState ClientState
  deriving (Eq, Show, Typeable)

data ClientState = ClientState {
    csTrash :: [Card],
    csMelds :: [[(Text,Card)]],
    csHand :: [Card] }
  deriving (Eq, Show)

instance ToJSON ClientState where
  toJSON (ClientState {..}) = object ["trash" .= csTrash, "melds" .= csMelds, "hand" .= csHand]

instance FromJSON ClientState where
  parseJSON (Object o) =
    ClientState
      <$> o .: "trash"
      <*> o .: "melds"
      <*> o .: "hand"

addPair k v (Object o) = Object $ H.insert k v o
addPair _ _ _ = error "Unexpected: addPair on not-an-object"

instance ToJSON Message where
  toJSON (Hello name) = object ["event" .= ("hello" :: Text), "name" .= name]
  toJSON Quit = object ["event" .= ("quit" :: Text)]
  toJSON OK = object ["event" .= ("ok" :: Text)]
  toJSON Cancel = object ["event" .= ("cancel" :: Text)]
  toJSON Start = object ["event" .= ("start" :: Text)]
  toJSON Wait = object ["event" .= ("wait" :: Text)]
  toJSON Processing = object ["event" .= ("processing" :: Text)]
  toJSON Lock = object ["event" .= ("lock" :: Text)]
  toJSON Unlock = object ["event" .= ("unlock" :: Text)]
  toJSON (Scores player scores) = object ["event" .= ("scores" :: Text), "scores" .= scores, "player" .= player]
  toJSON (GiveCard card) = object ["event" .= ("give" :: Text), "card" .= card]
  toJSON (MoveMsg i msg) = object ["event" .= ("move" :: Text), "player" .= i, "message" .= msg]
  toJSON (MoveAction i a) = addPair "event" "action" $ addPair "player" (toJSON i) $ toJSON a
  toJSON (Error err st) = object ["event" .= ("error" :: Text), "message" .= err, "state" .= st]
  toJSON (SetState st) = object ["event" .= ("state" :: Text), "state" .= st]

instance ToJSON LocalizedString where
  toJSON (Untranslated t) = toJSON t
  toJSON l = toJSON $ show l

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
  toJSON (ChangeJoker color i x) = object $ ["type" .= ("change" :: Text), "change" .= color, "meld" .= i] ++
                                            case x of
                                              Nothing -> []
                                              Just card -> ["card" .= card]
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
      "cancel" -> return Cancel
      "start" -> return Start
      "wait" -> return Wait
      "processing" -> return Processing
      "quit" -> return Quit
      "lock" -> return Lock
      "unlock" -> return Unlock
      "scores" -> Scores
                    <$> o .: "player"
                    <*> o .: "scores"
      "move" -> MoveMsg
                  <$> o .: "player"
                  <*> o .: "message"
      "action" -> MoveAction
                    <$> o .: "player"
                    <*> parseAction o
      "give" -> parseGive o
      "error" -> Error
                   <$> o .: "message"
                   <*> o .: "state"
      "state" -> SetState <$> (ClientState
                   <$> o .: "trash"
                   <*> o .: "melds"
                   <*> o .: "hand" )
  parseJSON x = fail $ "Invalid message object: " ++ show x

instance FromJSON LocalizedString where
  parseJSON (String t) = return $ Untranslated $ TL.fromStrict t
  parseJSON x = fail $ "Invalid message: " ++ show x

instance FromJSON MoveAction where
  parseJSON (Object o) = parseAction o
  parseJSON x = fail $ "Invalid object for move action: " ++ show x

parseChangeJoker :: Object -> Parser (Maybe (CardColor, MeldId, Maybe Card))
parseChangeJoker o = do
   chgj <- o .:? "change"
   case chgj of
     Nothing -> return Nothing
     Just color -> do
                   i <- o .: "meld"
                   x <- o .:? "card"
                   return $ Just (color, i, x)

parseMove o = do
  changeJoker <- parseChangeJoker o
  let chg = case changeJoker of
              Nothing -> Nothing
              Just (clr,i,_) -> Just (clr, i)
  pick <- o .:? "pick"
  meldCards <- o .:? "melds" .!= []
  adds <- o .:? "add" .!= []
  trash <- o .: "trash"
  melds <- forM meldCards $ \cards ->
             case buildMeld undefined cards of
               Left err -> fail $ "Invalid meld: " ++ show (err :: LocalizedString)
               Right meld -> return meld
  return $ Move chg pick melds adds trash

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
                  Just (color,i,x) -> return $ ChangeJoker color i x
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

getRequestLanguage :: WS.Request -> LanguageId
getRequestLanguage rq =
    case List.lookup (CI.mk "Accept-Language") (WS.requestHeaders rq) of
      Nothing -> "C"
      Just hdr -> let bstr = B.takeWhile ok hdr
                  in  map (chr . fromIntegral) (B.unpack bstr)
  where
    ok x = chr (fromIntegral x) `notElem` ",-;"

instance Protocol Message where
  type ProtocolState Message = (Translations, Chan Message, Chan Message)

  initProtocol _ = return ()

  onClientMessage rq sink msg (trans, fromPlayer, toPlayer) = do
    putStrLn $ "onClientMessage: " ++ show msg
    writeChan toPlayer msg
    res <- readChan fromPlayer
    putStrLn $ "Server answer: " ++ show res
    let lang = getRequestLanguage rq
    let res' = translateMessage trans lang res
    sendMessage sink res'

  getHelloUsername (Hello name) = Just name
  getHelloUsername _ = Nothing

  isQuit Quit = True
  isQuit _ = False
  
translateMessage :: Translations -> LanguageId -> Message -> Message
translateMessage t lang (MoveMsg i msg) = MoveMsg i $ Untranslated (translate' t lang msg)
translateMessage t lang (Error msg x) = Error (Untranslated $ translate' t lang msg) x
translateMessage _ _ x = x

