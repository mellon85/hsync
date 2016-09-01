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
      stopDHT,
      DHTID,
      DHT
    ) where

import Control.Exception

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import System.Random

import Data.Map.Strict as Map
import Data.Typeable (Typeable)

import Control.Monad

-- Network
import Network.Socket

-- Hash
import Crypto.Hash
import Data.ByteArray

-- STM
import Control.Concurrent.STM.TChan

-- | Callback for receiving data from the DHT C implementation
type Callback = CString -- ^ closure
            -> CInt     -- ^ event
            -> CString  -- ^ info_hash
            -> CString  -- ^ data (4 byte ip + 2 port in network byte order)
            -> CUInt    -- ^ data length (16 bytes ip + 2 byte port network byte order)
            -> IO ()

-- | Size (in bytes) of a DHT address
dhtIDSize = 20

-- | Pointer representing the DHTID address
type DHTID = ForeignPtr CChar

type PtrDHTID = Ptr CChar

foreign import ccall safe "ffi_run_dht" runDHT_ :: CInt -> CInt -> CShort ->
    PtrDHTID -> FunPtr Callback -> CString -> IO CInt
foreign import ccall safe "ffi_stop_dht" stopDHT :: IO ()
foreign import ccall safe "ffi_search" ffi_search :: PtrDHTID -> IO CInt
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

-- |Core representation of a DHT
newtype DHT = DHT {
        searches :: Map.Map DHTID (TChan SearchResult)
    }

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
       -> FunPtr Callback       -- ^ Function Pointer to a callback (TODO use STM to pass data)
       -> String                -- ^ Filepath with bootstrap nodes
       -> IO DHT                -- ^ Return code (use exception?)
runDHT v4 v6 port dht_id callback path = do
    fd4 <- makeSocket v4 port
    r <- withPersistSocket fd4 (\fd4 -> do
        fd6 <- makeSocket6 v6 port
        withPersistSocket fd6 (\fd6 ->
            withForeignPtr dht_id (\id ->
                withCString path $ runDHT_ fd4 fd6 portC id callback)))
    -- check r for exceptions
    if fromIntegral r /= 0
    then throw . Fail $ fromIntegral r
    else return DHT { searches = Map.empty }
    where
        portC = CShort $! fromIntegral port
        size = 20

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
    ptr <- mallocForeignPtrArray dhtIDSize
    ret <- withForeignPtr ptr (\dhtid ->
        -- fill ID with random data
        Prelude.take dhtIDSize . randoms <$> getStdGen >>= pokeArray dhtid)
    return ptr

-- |Searches for the specific DHT ID
--
search :: DHT       -- ^ DHT Instance
       -> DHTID     -- ^ DHT ID to look for
       -> IO (DHT, TChan SearchResult) -- ^ new DHT structure and the channel 
                                       -- to listen to
search dht dst = do
    tchan <- newTChanIO
    ret <- withForeignPtr dst ffi_search
    case fromIntegral ret of
        1 -> return (dht { searches = Map.insert dst tchan (searches dht)}, tchan)
        _ -> throw . Fail $ fromIntegral ret

