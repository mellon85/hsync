{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, FlexibleInstances #-}

-- TODO should use IORef.. switch to a proper logging library

--- {-# LANGUAGE UndecidableInstances #-}

-- use an MVar to replace the logfile synchronous with the processing

module Logger
    (runLoggerT,
     LoggerT(..),
     Level(..),
     log,
     logDebug,
     logError,
     logScope) where

import Prelude hiding(log)
import System.IO
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Concurrent
import Control.Exception
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Time
import Data.Time.Format
import Data.Typeable

import Control.Applicative

#if __GLASGOW_HASKELL__ <= 706
import System.Locale
#endif

-- TODO add StateT with the last timestamp logging (for rotation)

-- entities that can be logged
class Loggable a where
    getLogData :: a -> T.Text

-- Instance for Text
instance Loggable T.Text where
    getLogData = id
    {-# INLINE getLogData #-}

-- Instance for String
instance Loggable [Char] where
    getLogData = T.pack
    {-# INLINE getLogData #-}

-- Debug levels
data Level = DEBUG
           | INFO
           | WARN
           | ERROR
    deriving (Show, Eq, Ord)

data Message = Log T.Text  -- Message to log
             | Rotate      -- Rotate logfile (reopen)
             | Close       -- Stop the logging thread

type Channel = MVar Message

-- Logger configuration to be put in the internal Monad Reader
data LoggerConfiguration = LoggerConfiguration {
        level :: Level,
        channel :: Channel,
        barrier :: MVar (),
        logPath :: String,
        scope :: T.Text
    }

-- My logger
newtype LoggerT m a = LoggerT_ {
        runLoggerT_ :: ReaderT LoggerConfiguration m a
    }
    deriving (Functor, Applicative, Monad, MonadIO,
            MonadReader LoggerConfiguration)

runLoggerT (lvl, path) f = do
    var <- newEmptyMVar
    barrier_ <- newEmptyMVar
    let conf = LoggerConfiguration {
        level=lvl, channel=var, barrier=barrier_,
        logPath=path, scope=(T.pack "") }

    thread <- forkIO $ logger conf
    ret <- runReaderT (runLoggerT_ f) conf
    putMVar var Close -- tell the logger thread to stop
    takeMVar $ barrier conf -- wait for termination
    return ret

_p1 = T.pack "["
_p2 = T.pack " "
_p3 = T.pack "]"
_p4 = T.pack " : "

logger :: LoggerConfiguration -> IO ()
logger conf = (`runContT` return) $ do
        ret <- callCC $ \end -> forever $ do
                ret <- liftIO $ logger'
                case ret of
                    Rotate -> return () -- rotate
                    Close  -> end () -- stop logger
        return ret
    where
        ignore :: IOException -> IO ()
        ignore _ = return ()

        loop h = (`runContT` return) $ do
            ret <- callCC $ \end -> forever $ do
                    logData <- liftIO . takeMVar $ channel conf
                    case logData of
                        Log logData -> do
                            liftIO $ TIO.hPutStrLn h logData `catch` ignore
                        Close -> end Close
                        Rotate -> end Rotate
            return ret

        logger' :: IO Message
        logger' = bracket
                (openFile (logPath conf) AppendMode)
                (\h -> hClose h >> putMVar (barrier conf) ())
                (\handle -> loop handle)

logPar :: (MonadIO m, Loggable a) => Level -> Channel -> a -> T.Text -> LoggerT m ()
logPar level c s scope_ =  do
    time <- liftIO $ T.pack <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime
    threadId <- liftIO $ T.pack <$> drop 9 <$> show <$> myThreadId
    msg <- return $ T.concat [_p1, time, _p2, threadId, _p4, scope_, _p3, _p2,
                              T.pack (show level), _p4, getLogData s]
    liftIO $ putMVar c (Log msg)
{-# INLINE logPar #-}

log :: (MonadIO m, Loggable str) => Level -> str -> LoggerT m ()
log l s = do
    conf <- ask
    case l < level conf of
        True -> return ()
        _    -> logPar l (channel conf) s (scope conf)

logScope :: (Monad m, Loggable t) => t -> LoggerT m a -> LoggerT m a
logScope name cont = local replaceScope cont
    where
        replaceScope x = x { scope = getLogData name }

logDebug :: (MonadIO m, Loggable str) => str -> LoggerT m ()
logDebug = log DEBUG
{-# INLINE logDebug #-}

logError :: (MonadIO m, Loggable str) => str -> LoggerT m ()
logError = log ERROR
{-# INLINE logError #-}

logInfo :: (MonadIO m, Loggable str) => str -> LoggerT m ()
logInfo = log INFO
{-# INLINE logInfo #-}

logWarn :: (MonadIO m, Loggable str) => str -> LoggerT m ()
logWarn = log WARN
{-# INLINE logWarn #-}

