{-# LANGUAGE DeriveGeneric #-}

module Config where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import System.Directory (doesFileExist)

import State (HTHState(..))

data Config =
  Config {habitsFilename :: String, habitsFolder :: String}
  deriving (Generic, Show)

instance Aeson.FromJSON Config

defaultConfig = Config{habitsFilename = "habits", habitsFolder = "."}

configRead :: String -> IO Config
configRead filename = do
  cfgString <- BS.readFile filename
  return $ maybe defaultConfig id $ Aeson.decode cfgString

makeConfig :: String -> IO Config
makeConfig filename = do
  cfgFileExists <- doesFileExist filename
  if cfgFileExists
  then configRead filename
  else return defaultConfig

configureState :: HTHState -> IO HTHState
configureState st = do
  cfg <- makeConfig "hth.json"
  let filename = habitsFilename cfg
  let folder = habitsFolder cfg
  return st{configFilename = filename, configFolder = folder}
