{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module AI.Config where

import Control.Applicative
import Text.Printf
import Data.Aeson
import Data.Yaml
import Data.Maybe
import System.Directory

data Config = Config {
  onDebut :: Section,
  onMittelspiel :: Section,
  onEndspiel :: Section,
  avgPoints :: Double,
  nGames :: Int }
  deriving (Eq, Show)

data Section = Section {
    myHandNobodyCanExitCoef :: Double,
    myHandOneCanExitCoef :: Double,
    myHandSizeNobodyCanExitCoef :: Double,
    myHandSizeOneCanExitCoef :: Double,
    otherPointsCoef :: Double,
    potentialPointsCoef :: Double,
    bonusCoef :: Double }
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {
  onDebut = Section {
              myHandNobodyCanExitCoef = 0.5,
              myHandOneCanExitCoef = 1,
              myHandSizeNobodyCanExitCoef = 0.3,
              myHandSizeOneCanExitCoef = 0.1,
              otherPointsCoef = 0.3,
              potentialPointsCoef = 0.7,
              bonusCoef = 0.1 },
  onMittelspiel = Section {
              myHandNobodyCanExitCoef = 0.7,
              myHandOneCanExitCoef = 1,
              myHandSizeNobodyCanExitCoef = 0.3,
              myHandSizeOneCanExitCoef = 0.0,
              otherPointsCoef = 0.5,
              potentialPointsCoef = 0.5,
              bonusCoef = 0.7 },
  onEndspiel = Section {
              myHandNobodyCanExitCoef = 0.9,
              myHandOneCanExitCoef = 1,
              myHandSizeNobodyCanExitCoef = 0.1,
              myHandSizeOneCanExitCoef = 0.0,
              otherPointsCoef = 0.7,
              potentialPointsCoef = 0.3,
              bonusCoef = 1 },
  avgPoints = 0.0,
  nGames = 0
  }


instance FromJSON Section where
  parseJSON (Object o) =
    Section
      <$> o .: "my-hand-nobody-can-exit"
      <*> o .: "my-hand-one-can-exit"
      <*> o .:? "my-hand-size-nobody-can-exit" .!= 0.2
      <*> o .: "my-hand-size-one-can-exit" .!= 0.1
      <*> o .: "other-points"
      <*> o .: "potential-points"
      <*> o .: "bonus"
  parseJSON x = fail $ "Invalid object for Section: " ++ show x

instance FromJSON Config where
  parseJSON (Object o) =
    Config
      <$> o .: "debut"
      <*> o .: "mittelspiel"
      <*> o .: "endspiel"
      <*> o .: "avgpoints"
      <*> o .: "games"
  parseJSON x = fail $ "Invalid object for Config: " ++ show x

instance ToJSON Section where
  toJSON (Section {..}) =
    object ["my-hand-nobody-can-exit" .= myHandNobodyCanExitCoef,
            "my-hand-one-can-exit"    .= myHandOneCanExitCoef,
            "my-hand-size-nobody-can-exit" .= myHandSizeNobodyCanExitCoef,
            "my-hand-size-one-can-exit"    .= myHandSizeOneCanExitCoef,
            "other-points" .= otherPointsCoef,
            "potential-points" .= potentialPointsCoef,
            "bonus" .= bonusCoef ]

instance ToJSON Config where
  toJSON (Config {..}) =
    object ["debut" .= onDebut,
            "mittelspiel" .= onMittelspiel,
            "endspiel" .= onEndspiel,
            "avgpoints" .= avgPoints,
            "games" .= nGames ]

load :: Int -> IO Config
load i = do
  let filename = printf "ai.%d.yaml" i
  b <- doesFileExist filename
  if b
    then do
         mbr <- decodeFile filename
         return $ fromMaybe defaultConfig mbr
    else do
         putStrLn $ printf "No config file %s, using default config" filename
         return defaultConfig

save :: Int -> Config -> IO ()
save i config = do
  let filename = printf "ai.%d.yaml" i
  encodeFile filename config

