{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : DHT
Description : DHT Interface
Copyright   : (c) Dario Meloni, 2016
License     : GPL-3
Maintainer  : mellon85@gmail.com
Stability   : experimental

Interface module to the C implementation of the DHT.
-}

module DHT
    ( runDHT,
      generateID,
      withDHT,
      stopDHT,
      DHTID,
      DHT
    ) where

import Control.Exception

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import System.Random

import Data.Map.Strict as Map
import Data.Typeable (Typeable)

import Control.Monad
import Data.Maybe (isJust)

-- Network
import Network.Socket

-- Hash
import Crypto.Hash
import Data.ByteArray

-- STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar

-- | Callback for receiving data from the DHT C implementation
type FFICallback = CString  -- ^ closure
                -> CInt     -- ^ event
                -> CString  -- ^ info_hash
                -> CString  -- ^ data (4 byte ip + 2 port in network byte order)
                -> CUInt    -- ^ data length (16 bytes ip + 2 byte port network byte order)
                -> IO ()

-- | Size (in bytes) of a DHT address
dhtIDSize = 20

-- | Pointer representing the DHTID address
type DHTID = Ptr CChar

foreign import ccall unsafe "ffi_run_dht" runDHT_ :: CInt -> CInt -> CShort ->
    DHTID -> FunPtr FFICallback -> CString -> IO CInt
foreign import ccall unsafe "ffi_stop_dht" stopDHT_ :: IO ()
foreign import ccall unsafe "ffi_search" ffi_search :: DHTID -> IO CInt
foreign import ccall safe "ffi_get_nodes" getNodes :: Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall safe "ffi_add_node" addNode :: Ptr () -> CShort -> IO ()

-- |Utility for CInt and so on. Convert an integer from any to any type
conv = fromIntegral . toInteger

type DHTHashCall = Ptr CChar -> CInt -> -- Hash Return
                   Ptr CChar -> CInt -> -- Buffer 1
                   Ptr CChar -> CInt -> -- Buffer 2
                   Ptr CChar -> CInt -> IO () -- Buffer 3

-- |Represent a search result
data SearchResult = End
                  | IP4 HostAddress Int
                  | IP6 HostAddress6 Int

data DHT_ = DHT_ {
        searches :: MVar (Map.Map DHTID (TChan SearchResult)),
        callback :: FunPtr FFICallback,
        ipv4 :: Bool,
        ipv6 :: Bool
    }

-- |Core representation of a DHT
newtype DHT = DHT DHT_

-- |dht_hash implementation in haskell
foreign export ccall dht_hash :: DHTHashCall

-- |Function called from the dht C-code to hash all data and copy it in the
-- output buffer
dht_hash :: DHTHashCall
dht_hash dst dstl in1 in1l in2 in2l in3 in3l = do
    digest <- return $! hash_all
    withByteArray digest (\ptr -> copyBytes dst ptr (conv dstl))
    return ()
    where
        in_1 = MemView (castPtr in1) (conv in1l)
        in_2 = MemView (castPtr in2) (conv in2l)
        in_3 = MemView (castPtr in3) (conv in3l)

        hash_all :: Digest SHA1
        hash_all = hashFinalize . hashUpdates hashInit $ [in_1, in_2, in_3]

data DHTException = Fail Int
    deriving (Show, Typeable)

instance Exception DHTException

-- |Runs the DHT and returns an error code (0 means everything's ok)
-- Return an exception from an error code
runDHT :: Maybe HostAddress     -- ^ IPv4 address
       -> Maybe HostAddress6    -- ^ IPv6 address
       -> Int                   -- ^ Port number
       -> DHTID                 -- ^ DHT ID
       -> String                -- ^ Filepath with bootstrap nodes
       -> IO DHT                -- ^ Return DHT structure (use exception?)
runDHT v4 v6 port dht_id path = do
    dht_@(DHT dht) <- makeDHT
    safeDHT dht $ do
        fd4 <- makeSocket v4 port
        r <- withPersistSocket fd4 (\fd4 -> do
            fd6 <- makeSocket6 v6 port
            withPersistSocket fd6 (\fd6 ->
                withCString path $ runDHT_ fd4 fd6 portC dht_id (callback dht)))
        -- check r for exceptions
        if fromIntegral r /= 0
            then throw . Fail $ fromIntegral r
            else return dht_

    where
        portC = CShort $! fromIntegral port

        makeDHT = do
            entries <- newMVar Map.empty
            let dht = DHT_ {
                searches = entries,
                callback = nullFunPtr,
                ipv4 = isJust v4,
                ipv6 = isJust v6 }
                in do
                callb <- mkCallback (ffiCallback dht )
                return . DHT $ dht { callback = callb }
        safeDHT d f = f `onException` cleanupDHT d

-- | Safely use a DHT
withDHT :: t -> IO DHT_ -> (DHT_ -> IO c) -> IO c
withDHT d f = bracket f stopDHT

-- Clean up all memory associated with the DHT structure
cleanupDHT :: DHT_ -> IO ()
cleanupDHT dht =
    freeHaskellFunPtr $ callback dht

stopDHT :: DHT_ -> IO()
stopDHT dht = do
    stopDHT_
    cleanupDHT dht

-- |Utility functions to create a socket IPv4
makeSocket :: Maybe HostAddress -> Int -> IO (Maybe Socket)
makeSocket Nothing port = return Nothing
makeSocket (Just host) port = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock $ SockAddrInet (fromIntegral port) host
    return $ Just sock

-- |Utility functions to create a socket IPv6
makeSocket6 :: Maybe HostAddress6 -> Int -> IO (Maybe Socket)
makeSocket6 Nothing port = return Nothing
makeSocket6 (Just host) port = do
    sock <- socket AF_INET6 Stream defaultProtocol
    bind sock $ SockAddrInet6 (fromIntegral port) 0 host 0
    return $ Just sock

-- |Executes an action on the filedescriptor of the Socket
-- As the dht is owning the sockets the caller has to close them only in case of
-- an exception.
withPersistSocket :: Maybe Socket -> (CInt -> IO a) -> IO a
withPersistSocket sock f = f (maybe (CInt $ negate 1) fdSocket sock) `onException`
    maybe (return ()) close sock

-- |generates a random DHTID
generateID :: IO DHTID
generateID = do
    ptr <- mallocArray dhtIDSize
    -- fill ID with random data
    Prelude.take dhtIDSize . randoms <$> getStdGen >>= pokeArray ptr
    return ptr

-- |Searches for the specific DHT ID
--
search :: DHT       -- ^ DHT Instance
       -> DHTID     -- ^ DHT ID to look for
       -> IO (TChan SearchResult) -- ^ new DHT structure and the channel to listen to
search (DHT dht) dst = do
    tchan <- newTChanIO
    ret <- ffi_search dst
    case fromIntegral ret of
        1 -> modifyMVar_ (searches dht) (return . Map.insert dst tchan) >> return tchan
        _ -> throw . Fail $ fromIntegral ret

ffiCallback :: DHT_ -> FFICallback
ffiCallback dht _ event hash addr len = do
    mchan <- withMVar (searches dht) $ return . Map.lookup hash
    maybe (return ()) (\chan ->
        case event of
            0 -> return () -- no event
            1 -> -- ipv4
                return ()
            2 -> -- ipv6
                return ()
            -- in case of 3 and 4 we have to check if we supported the other
            -- protocol or not. In case it was then we have to check if the
            -- other search has finished too. (TODO need to store that
            -- information somewhere
            3 -> -- done ipv4
                return ()
            4 -> -- done ipv6
                return ()
            _ -> return () -- Unknown event, ignore and hope for the best
        ) mchan

-- |Wrapper to pass the callback function to the C layer
foreign import ccall "wrapper"
    mkCallback :: FFICallback -> IO (FunPtr FFICallback)
