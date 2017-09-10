{-# LANGUAGE OverloadedStrings #-}

module Configuration (
    Configuration(..),
    DHTConf(..),
    BroadcastConf(..),
    LoggerConf(..),
    readDefaultConfiguration,
    readConfiguration,
    ) where

import qualified Logger as L
import Logger (infoM, errorM)

import System.IO as IO (stderr, hPutStrLn)

import qualified Data.Text as T
import Data.Word
import System.Directory
import Data.Maybe (fromJust)
import Control.Monad
import Control.Exception
import Control.Applicative
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as BSC

import qualified Data.Text.IO as TO
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Yaml
import Data.Yaml.Config
import Data.Semigroup ((<>))

import qualified Data.ByteString as BS

---- Main Configuration
data Configuration = Conf {
        logging :: LoggerConf
      , dht :: DHTConf
      , broadcast :: BroadcastConf
    } deriving (Show)

instance FromJSON Configuration where
    parseJSON = withObject "" $ \v -> Conf
        <$> v .: "logging"
        <*> v .: "dht"
        <*> v .: "broadcast"

instance ToJSON Configuration where
    toJSON confs = object [
        "dht"       .= dht confs
      , "broadcast" .= broadcast confs
      , "logging"   .= logging confs
        ]


---- DHT Configuration

data DHTConf = DHTConf {
        dht_enabled :: Bool
      , dht_port :: Word16
    } deriving(Show)

instance FromJSON DHTConf where
    parseJSON = withObject "dht" $ \v -> DHTConf
        <$> v .:? "enabled" .!= True
        <*> v .:? "port"    .!= 4545

instance ToJSON DHTConf where
    toJSON (DHTConf _enabled _port) = object [
        "enabled" .= _enabled
      , "port"    .= _port
        ]


---- Broadcast configuration

data BroadcastConf = BroadcastConf {
        broadcastEnabled :: Bool
      , broadcastPort :: Word16
      , broadcastInterval :: Word32
    } deriving(Show)

instance FromJSON BroadcastConf where
    parseJSON = withObject "broadcast" $ \v -> BroadcastConf
        <$> v .:? "enabled"  .!= True
        <*> v .:? "port"     .!= 4546
        <*> v .:? "interval" .!= 60

instance ToJSON BroadcastConf where
    toJSON (BroadcastConf _enabled _port _interval) = object [
        "enabled"  .= _enabled
      , "port"     .= _port
      , "interval" .= _interval
        ]

defaultConf = BSC.pack $
              "logging: {}\n"
           ++ "dht: {}\n"
           ++ "broadcast: {}\n"

---- Logger configuration

data LoggerConf = LoggerConf {
        loggingLevel :: L.Priority
    } deriving(Show)

instance FromJSON LoggerConf where
    parseJSON = withObject "logging" $ \v -> LoggerConf
        <$> v .:? "level"  .!= L.DEBUG

instance ToJSON LoggerConf where
    toJSON (LoggerConf _level) = object [
        "level"  .=  _level
        ]

-- | Instance for logging priorities
instance ToJSON L.Priority where
    toJSON = String . T.pack . show

-- | Instance for logging priorities
instance FromJSON L.Priority where
    parseJSON (String t) = read <$> (pure . T.unpack $ t)
    parseJSON wat = typeMismatch "Log level" wat

defaultConfigurationPath = "~/.hsync"

readDefaultConfiguration :: IO Configuration
readDefaultConfiguration = readConfiguration defaultConfigurationPath

configurationPaths = ["hsync.yaml", "~/.hsync.yaml"]

readConfiguration path = do
    paths <- filterM doesFileExist configurationPaths
    conf <- loadYamlSettings paths [(fromJust $ decode defaultConf)] useEnv
    print conf
    return conf
