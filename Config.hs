{-# LANGUAGE DeriveGeneric #-}
module Config where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List.Split (splitOn)
import GHC.Generics
import System.Directory (findFile)
import System.Environment (lookupEnv)

import State (HTHState(..))

data Config =
  Config {habitsFilename :: String, habitsFolder :: String}
  deriving (Generic, Show)

instance Aeson.FromJSON Config

defaultConfig = Config{habitsFilename = "habits", habitsFolder = "."}

-- Read config from file.
configRead :: String -> IO Config
configRead filename = do
  cfgString <- BS.readFile filename
  return $ maybe defaultConfig id $ Aeson.decode cfgString

-- Look for config file in current directory or Path.
findConfig :: IO (Maybe String)
findConfig = do
  path <- lookupEnv "PATH"
  case path of
    Nothing -> searchPaths "."
    (Just folders) -> searchPaths $ ".;" ++ folders

-- Helper function for findConfig.
searchPaths :: String -> IO (Maybe String)
searchPaths path = do
  let folders = splitOn ";" path -- Not tested on Unix.
  findFile folders "hth.json"

-- Make config, using file if found in current directory or Path.
makeConfig :: IO Config
makeConfig = do
  file <- findConfig
  case file of
    Nothing -> return defaultConfig
    (Just filename) -> configRead filename

-- Add config to state.
configureState :: HTHState -> IO HTHState
configureState st = do
  cfg <- makeConfig
  let filename = habitsFilename cfg
  let folder = habitsFolder cfg
  return st{configFilename = filename, configFolder = folder}
