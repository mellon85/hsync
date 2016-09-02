{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell #-}

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
      DHT,
      search,
      nodes,
      addNode
    ) where

import Control.Exception

import Data.Word (Word16, Word32)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import Control.Monad.STM

import System.Random
import Data.IP

import Data.Map.Strict as Map
import Data.Typeable (Typeable)

import Control.Monad
import Data.Maybe (isJust)

import Data.Bits (shiftR, shiftL, (.&.))

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

foreign import ccall safe "ffi_run_dht" ffi_run_dht :: CInt -> CInt -> CShort ->
    DHTID -> FunPtr FFICallback -> CString -> IO CInt
foreign import ccall safe "ffi_stop_dht" ffi_stop_dht :: IO ()
foreign import ccall safe "ffi_search" ffi_search :: DHTID -> IO CInt
foreign import ccall safe "ffi_get_nodes" ffi_get_nodes :: Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall safe "ffi_add_node_4" ffi_add_node_4 :: Ptr () -> CShort -> IO ()
foreign import ccall safe "ffi_add_node_6" ffi_add_node_6 :: Ptr () -> CShort -> IO ()

foreign import ccall unsafe "ntohs" ntohs :: Word16 -> Word16

-- |Utility for CInt and so on. Convert an integer from any to any type
conv = fromIntegral . toInteger

type DHTHashCall = Ptr CChar -> CInt -> -- Hash Return
                   Ptr CChar -> CInt -> -- Buffer 1
                   Ptr CChar -> CInt -> -- Buffer 2
                   Ptr CChar -> CInt -> IO () -- Buffer 3

-- |Represent a search result
data SearchResult = End
                  | IP4 HostAddress Word16
                  | IP6 HostAddress6 Word16

data DHT_ = DHT_ {
        searches :: MVar (Map.Map DHTID (TChan SearchResult, Int)),
        callback :: FunPtr FFICallback,
        protoCount :: Int
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
                withCString path $ ffi_run_dht fd4 fd6 portC dht_id (callback dht)))
        -- check r for exceptions
        if fromIntegral r /= 0
            then throw . Fail $ fromIntegral r
            else return dht_

    where
        portC = fromIntegral port

        makeDHT = do
            entries <- newMVar Map.empty
            let dht = DHT_ {
                searches = entries,
                callback = nullFunPtr,
                protoCount = fromEnum (isJust v4) + fromEnum (isJust v6)
            }
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

-- |Stop the DHT and clear all channels on the DHT side
stopDHT :: DHT_ -> IO()
stopDHT dht = do
    ffi_stop_dht
    modifyMVar_ (searches dht) (\_ -> pure Map.empty) -- zero the map of searches
    cleanupDHT dht -- useful to also clear the circular dependency in the DHT

-- |Utility functions to create a socket IPv4
makeSocket :: Maybe HostAddress -> Int -> IO (Maybe Socket)
makeSocket Nothing port = return Nothing
makeSocket (Just host) port = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock $ SockAddrInet (fromIntegral port) host
    listen sock 10
    return $ Just sock

-- |Utility functions to create a socket IPv6
makeSocket6 :: Maybe HostAddress6 -> Int -> IO (Maybe Socket)
makeSocket6 Nothing port = return Nothing
makeSocket6 (Just host) port = do
    sock <- socket AF_INET6 Stream defaultProtocol
    bind sock $ SockAddrInet6 (fromIntegral port) 0 host 0
    listen sock 10
    return $ Just sock

-- |Executes an action on the filedescriptor of the Socket
-- As the dht is owning the sockets the caller has to close them only in case of
-- an exception.
withPersistSocket :: Maybe Socket -> (CInt -> IO a) -> IO a
withPersistSocket sock f = f (maybe (CInt $ negate 1) fdSocket sock) `onException`
    maybe (return ()) close sock

-- |Generates a random DHTID
generateID :: IO DHTID
generateID = do
    ptr <- mallocArray dhtIDSize
    -- fill ID with random data
    Prelude.take dhtIDSize . randoms <$> getStdGen >>= pokeArray ptr
    return ptr

-- |Searches for the specific DHT ID
search :: DHT       -- ^ DHT Instance
       -> DHTID     -- ^ DHT ID to look for
       -> IO (TChan SearchResult) -- ^ new DHT structure and the channel to listen to
search (DHT dht) dst = do
    tchan <- newTChanIO
    ret <- ffi_search dst
    case fromIntegral ret of
        1 -> do
            modifyMVar_ (searches dht) $ pure . Map.insert dst (tchan, 0)
            return tchan
        _ -> throw . Fail $ fromIntegral ret

-- |Callback from DHT
ffiCallback :: DHT_ -> FFICallback
ffiCallback dht _ event hash addr len = do
    mchan <- withMVar (searches dht) $ return . Map.lookup hash
    maybe (return ()) (\(chan, count) -> 
        case event of
            0 -> return () -- no event
            1 -> do -- ipv4 found
                -- addr is the ipv4 address and port in host byte order
                ipv4addr <- peek $ castPtr addr :: IO HostAddress
                port <- peekByteOff  addr (sizeOf ipv4addr) :: IO Word16
                atomically . writeTChan chan . IP4 ipv4addr $ ntohs port
                return ()
            2 -> do -- ipv6
                -- addr is the ipv6 address and port in host byte order
                [a,b,c,d] <- peekArray 4 $ castPtr addr :: IO [Word32]
                port <- peekByteOff  addr (4* sizeOf a) :: IO Word16
                atomically . writeTChan chan . IP6 (a,b,c,d) $ ntohs port
                return ()

            -- in case of 3 and 4 we have to check if we supported the other
            -- protocol or not. In case it was then we have to check if the
            -- other search has finished too.
            3 -> receivedEnd chan count
            4 -> receivedEnd chan count
            _ -> return () -- Unknown event, ignore and hope for the best
        ) mchan
    where
        receivedEnd chan count | count > 1 = decrease dht
                               | otherwise = do
                                    atomically . writeTChan chan $ End
                                    modifyMVar_ (searches dht) (pure . Map.delete hash)

        decrease dht = let
            decreaseElem = maybe Nothing (\(t,c) -> Just (t,c-1))
            in modifyMVar_ (searches dht) (pure . Map.alter decreaseElem hash)

-- |Wrapper to pass the callback function to the C layer
foreign import ccall "wrapper"
    mkCallback :: FFICallback -> IO (FunPtr FFICallback)


nodes :: IO (Int, Int)
nodes = do
    v4 <- malloc
    v6 <- malloc
    ffi_get_nodes v4 v6
    v4count <- peek v4
    v6count <- peek v6
    return (fromIntegral v4count,fromIntegral v6count)

addNode :: IP -> Word16 -> IO ()
addNode (IPv4 ip) port =
    allocaArray 4 (\ipPtr -> do
        poke ipPtr (toHostAddress ip)
        ffi_add_node_4 (castPtr ipPtr) $ fromIntegral port)
addNode (IPv6 ip) port =
    allocaArray 16 (\ipPtr -> do
        poke ipPtr (toHostAddress6 ip)
        ffi_add_node_6 (castPtr ipPtr) $ fromIntegral port)

