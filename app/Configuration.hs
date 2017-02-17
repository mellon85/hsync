{-# LANGUAGE DeriveGeneric #-}

module Configuration (
    Configuration(..),
    readDefaultConfiguration,
    readConfiguration,
    defaultConfig,
    ) where

import GHC.Generics
import qualified Logger as L
import qualified Data.Text as T
import Data.Word
import Data.Aeson.Types (typeMismatch)
import System.Directory
import Control.Monad
import Control.Exception
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL

import Data.Aeson
import qualified Data.ByteString as BS

data Configuration = Conf {
        dhtEnabled :: Bool,
        dhtPort :: Word16,
        broadcastEnabled :: Bool,
        broadcastPort :: Word16,
        broadcastRefresh :: Word32,
        loggerLevel :: L.Priority
    } deriving (Generic, Show)

instance ToJSON Configuration where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Configuration

-- | Instance for logging priorities
instance ToJSON L.Priority where
    toJSON = String . T.pack . show

-- | Instance for logging priorities  
instance FromJSON L.Priority where
    parseJSON (String t) = read <$> (pure . T.unpack $ t)
    parseJSON wat = typeMismatch "Log level" wat

defaultConfig = Conf {
        dhtEnabled = True,
        dhtPort = 4545,
        broadcastEnabled = True,
        broadcastPort = 4546,
        broadcastRefresh = 60,
        loggerLevel = L.DEBUG
    }

defaultConfigurationPath = "~/.hsync"

readDefaultConfiguration :: IO (Maybe Configuration)
readDefaultConfiguration = readConfiguration defaultConfigurationPath

readConfiguration path = do
    (createIfMissing >> safeRead) `catch` clean
    where
        createIfMissing = do
            exists <- doesFileExist path
            when (not exists) $ BSL.writeFile path (encode defaultConfig)

        safeRead = do
            f <- BSL.readFile path
            return $ decode f

        clean :: SomeException -> IO (Maybe Configuration)
        clean _ = return Nothing

