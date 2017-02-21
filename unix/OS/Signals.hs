module OS.Signals
    (installSignalHandlers)
    where

import System.Posix.Signals

installSignalHandlers :: IO () -> IO ()
installSignalHandlers signal = do
    installHandler sigTERM (CatchOnce $ signal) Nothing
    installHandler sigINT (CatchOnce $ signal) Nothing
    return ()
