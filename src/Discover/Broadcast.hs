module Discover.Broadcast (
    start,
    stop,
    ip4Running,
    ip6Running,
    Broadcast) where

import Prelude hiding (catch)
import Network.Socket hiding (send, sendAllTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Monad.STM
import Data.Time.Clock
import Control.Concurrent.STM.TChan
import Data.Maybe (isJust)
import Control.Monad.IO.Class
import Control.Monad
import System.Timeout
import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import Logger

logModule = "BRD"

data Broadcast = B {
        b_s4   :: Maybe (Socket, Async()),
        b_s6    :: Maybe (Socket, Async()),
        b_peers :: TChan SockAddr
    }

ip4Running :: Broadcast -> Bool
ip4Running = isJust . b_s4

ip6Running :: Broadcast -> Bool
ip6Running = isJust . b_s6

data BroadcastConfiguration = BC {
        refreshTime :: Int,             -- ^ Seconds to wait between beacons  
        addressChan :: TChan SockAddr,  -- ^ Channel to communicate addresses found
        family :: Family,               -- ^ Socket Family
        addr :: String,                 -- ^ Address to look for
        port :: PortNumber,             -- ^ Port number
        sock :: Socket                  -- ^ Socket
    }

instance Show BroadcastConfiguration where
    show (BC r _ f a _ _) = msg
        where
            msg = title . shows r . space . shows f . space . shows a $ []
            title = ("BroadcastConfiguration "++)
            space = (" "++)

debug = debugM logModule
info = infoM logModule
warning = warningM logModule

-- | Starts a Broadcast discovery thread
start ::
    PortNumber    -- ^ Port number to use
 -> Int           -- ^ Refresh interval in seconds
 -> IO Broadcast  -- ^ Broadcast control block
start p n = do
    info $ "Starting Broadcast system on port " ++ (show p)
    peers <- newTChanIO

    -- try to start IPv6 and then IPv4
    -- always report the situation anyway
    debug $ "Start IPv6 Broadcast"
    s6 <- bracketOnError
            (socket AF_INET6 Datagram defaultProtocol)
            (\s -> do
                close s
                info $ "Stop IPv6 Broadcast")
            (broadcast . BC n peers AF_INET6 "ff02::1" p)

    debug $ "Start IPv4 Broadcast"
    s4 <- bracketOnError
            (socket AF_INET Datagram defaultProtocol)
            (\s -> do
                close s
                info $ "Stop IPv6 Broadcast")
            (\s -> do
                when (isSupportedSocketOption Broadcast) $ setSocketOption s Broadcast 1
                broadcast $ BC n peers AF_INET "255.255.255.255" p s)

    return $ B s4 s6 peers

-- | Stop the Broadcast discovery
stop :: Broadcast -> IO ()
stop br = do
    info $ "Stop Broadcast"
    mapM_ (maybe (return ()) stop') [b_s4 br, b_s6 br]
    where
        stop' (s,a) = do
            close s
            wait a

broadcast ::
        BroadcastConfiguration        -- ^ Configurations
     -> IO (Maybe (Socket, Async ())) -- ^ The socket and the async computation
                                      --   handling the loop
broadcast bc@(BC r chan f addr p s) = broadcast' `catch` logError
    where
        logError e = do
            errorM logModule $ displayException (e :: SomeException)
            return Nothing

        broadcast' = do
            debug $ show bc
            addr <- getAddrInfo (Just hint) (Just addr) $ Just $ show p
            liftIO $ debug $ show addr
            if length addr == 0
            then do
                warning $ ("Failed to start "++) . shows bc $ []
                return Nothing
            else let address = head addr in do
                debug $ "Starting with address " ++ (show address)
                bind s $ addrAddress address
                a <- async $ broadcastLoop s r (addrAddress address) chan
                return $ Just (s, a)

        hint = defaultHints {
                    addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV],
                    addrFamily = f,
                    addrSocketType = Datagram,
                    addrProtocol = 0 }

-- payload = hsync \0 <protocol version as 16 bit>
payload = BS.pack [72, 83, 89, 78, 67, 00, 01]

broadcastLoop :: Socket -> Int -> SockAddr -> TChan SockAddr -> IO ()
broadcastLoop s r baddr peers = do
    runMaybeT (runStateT (forever go) 0)
    return ()
    where
        -- in case of stop we have to get out of the loop at the catch!
        go = go' `catch` (\e -> do
            liftIO $ errorM logModule "exception received"
            liftIO $ errorM logModule $ displayException (e :: SomeException)
            return ())

        go' = do
            cumulative <- get
            bound <- liftIO $ isBound s
            when (not bound) $ lift mzero
            (pkt, elapsed) <- liftIO $ recvBroadcast cumulative
            when ((elapsed + cumulative) >= r) $ liftIO sendBroadcast
            put $ max (elapsed + cumulative - r) 0
            c <- get
            case pkt of
                Just (s, addr) -> do
                    if BS.empty == s
                    then do
                        liftIO $ debug "Received stop for broadcast loop"
                        lift mzero -- interrupt loop
                    else do
                        liftIO $ do debug $ ("Found "++) . shows addr $ []
                                    atomically $ writeTChan peers addr
                Nothing -> do
                    liftIO $ debug "Broadcast timeout"

        recvBroadcast t = do
            startTime <- getCurrentTime
            rt <- timeout ((r - t)*1000000) $ recvFrom s 8192
            wakeTime <- getCurrentTime
            return (rt, truncate $ diffUTCTime wakeTime startTime)
        sendBroadcast = do
            liftIO $ debug "Send broadcast message"
            sendAllTo s payload baddr
            return ()

