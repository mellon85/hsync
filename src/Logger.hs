module Logger
    (setupLogger
    ,closeLogger
    ,rootLogger
    ,infoM
    ) where

import System.Log.Logger
import qualified System.Log.Handler as LH
import System.Log.Handler.Simple
import System.Log.Formatter (simpleLogFormatter)

setupLogger :: IO ()
setupLogger = do
    fileLog <- fileHandler "dht-sync.log" DEBUG >>= \lh ->
        return $ LH.setFormatter lh $
                simpleLogFormatter "$utcTime <$loggername> $tid [$prio] $msg"
    updateGlobalLogger "" removeHandler
    updateGlobalLogger "" $ addHandler fileLog
    updateGlobalLogger "" $ setLevel DEBUG
    debugM rootLogger "Started logging"

closeLogger :: IO ()
closeLogger = --do
    debugM rootLogger "Stopped logging"
    --updateGlobalLogger "" LH.close

rootLogger = rootLoggerName

