module Logger
    (setupLogger
    ,closeLogger
    ,rootLogger
    ,infoM
    ,warningM
    ,debugM
    ,errorM
    ,Priority(DEBUG, INFO, ERROR)
    ) where

import System.Log.Logger
import qualified System.Log.Handler as LH
import System.Log.Handler.Simple
import System.Log.Formatter (simpleLogFormatter)

setupLogger :: Priority -> IO ()
setupLogger prio = do
    fileLog <- fileHandler "dht-sync.log" DEBUG >>= \lh ->
        return $ LH.setFormatter lh $
                simpleLogFormatter "$utcTime <$loggername> $tid [$prio] $msg"
    updateGlobalLogger rootLogger removeHandler
    updateGlobalLogger rootLogger $ addHandler fileLog
    updateGlobalLogger rootLogger $ setLevel prio
    debugM rootLogger "Started logging"

closeLogger :: IO ()
closeLogger = --do
    debugM rootLogger "Stopped logging"
    --updateGlobalLogger "" LH.close

rootLogger = rootLoggerName

