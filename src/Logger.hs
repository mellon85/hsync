module Logger
    (setupLogger
    ,closeLogger
    ,rootLogger
    ,infoM
    ,warningM
    ,debugM
    ,errorM
    ,Priority(DEBUG, INFO, WARNING, ERROR)
    ) where

import System.Log.Logger
import qualified System.Log.Handler as LH
import System.Log.Handler.Simple
import System.Log.Formatter (simpleLogFormatter)
import System.IO (stdout)

logFormat = "$utcTime <$loggername> $tid [$prio] $msg"

setupLogger :: Priority -> IO ()
setupLogger prio = do
    fileLog <- fileHandler "dht-sync.log" DEBUG >>= \lh ->
        return $ LH.setFormatter lh $
                simpleLogFormatter logFormat

    console <- streamHandler stdout ERROR >>= \lh ->
        return $ LH.setFormatter lh $
                simpleLogFormatter logFormat

    updateGlobalLogger "" removeHandler
    updateGlobalLogger "" $ addHandler fileLog
    updateGlobalLogger "" $ addHandler console
    updateGlobalLogger "" $ setLevel prio
    debugM rootLogger "Started logging"

closeLogger :: IO ()
closeLogger = --do
    debugM rootLogger "Stopped logging"
    --updateGlobalLogger "" LH.close

rootLogger = rootLoggerName

