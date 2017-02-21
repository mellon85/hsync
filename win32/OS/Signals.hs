module OS.Signals
    (installSignalHandlers)
    where

import Foreign.Ptr
import System.Win32.Console.CtrlHandler

foreign import ccall "wrapper"
    mkSignalHandler :: Handler -> IO PHANDLER_ROUTINE

signalWrapper :: IO () -> Handler
signalWrapper signal _ = do -- we ignore the event
    signal
    return True

installSignalHandlers :: IO () -> IO ()
installSignalHandlers signal = do
    c_SetConsoleCtrlHandler (mkSignalHandler $ signalWrapper signal) True
    return ()

