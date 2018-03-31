{-# LANGUAGE DeriveGeneric #-}
module Config where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List.Split (splitOn)
import GHC.Generics
import System.Directory (findFile)
import System.Environment (lookupEnv)

data Config =
  Config {habitsFilename :: String, habitsFolder :: String}
  deriving (Generic, Show)

instance Aeson.FromJSON Config

defaultConfig = Config{habitsFilename = "habits", habitsFolder = "."}

-- Read config from file.
configRead :: String -> IO Config
configRead filename =
  BS.readFile filename >>=
  return . (maybe defaultConfig id) . Aeson.decode

-- Look for config file in current directory or Path.
findConfig :: IO (Maybe String)
findConfig =
  lookupEnv "PATH" >>= maybe (searchPaths ".") (searchPaths . (".;"++))

-- Helper function for findConfig.
-- Not tested on Unix.
searchPaths :: String -> IO (Maybe String)
searchPaths path =
  let folders = splitOn ";" path
  in findFile folders "hth.json"

-- Make config, using file if found in current directory or Path.
makeConfig :: IO Config
makeConfig = findConfig >>= maybe (return defaultConfig) configRead
