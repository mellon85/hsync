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
    ( generateID,
      startDHT,
      stopDHT,
      DHTID,
      search,
      announce,
      nodes,
      addNode,
      loadBootstrap,
      saveBootstrap,
      addRemoteBootstrapNodes,
      conditionalBootstrap
    ) where

import System.Directory
import Control.Exception

import Data.Word (Word16, Word32, Word64)
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

import Control.Concurrent (forkIO)
import Data.Map.Strict as Map
import Data.Typeable (Typeable)

import Control.Monad
import Data.Maybe (isJust)

import Data.Bits (shiftR, shiftL, (.&.))
import System.IO.Error

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (liftIO)

-- Network
import Network.Socket

-- Hash
import Crypto.Hash
import Data.ByteArray as BA

-- STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar

import Numeric

import Logger

logModule = "DHT"

-- | Callback for receiving data from the DHT C implementation
type FFICallback = CString  -- ^ closure
                -> CInt     -- ^ event
                -> CString  -- ^ info_hash
                -> CString  -- ^ data (4 byte ip + 2 port in network byte order)
                -> CUInt    -- ^ data length (16 bytes ip + 2 byte port network byte order)
                -> IO ()


data DHTID = DHTID Word64 Word64 Word32
    deriving (Eq, Ord)

instance Storable DHTID where
    sizeOf _ = 20
    alignment _ = 8
    peek p = do
        a <- peekByteOff (castPtr p) 0
        b <- peekByteOff (castPtr p) 8
        c <- peekByteOff (castPtr p) 16
        return $ DHTID a b c

    poke p (DHTID a b c) =
        pokeByteOff p 0  a >>
        pokeByteOff p 8  b >>
        pokeByteOff p 16 c

instance Show DHTID where
    show (DHTID a b c) = showHex a . showHex b . showHex c $ ""

foreign import ccall safe "ffi_run_dht" ffi_run_dht :: CInt -> CInt ->
    Ptr CChar -> FunPtr FFICallback -> IO CInt
foreign import ccall safe "ffi_stop_dht" ffi_stop_dht :: IO ()
foreign import ccall safe "ffi_search" ffi_search :: Ptr CChar -> CShort -> FunPtr FFICallback -> IO ()
foreign import ccall safe "ffi_get_nodes" ffi_get_nodes :: Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall safe "ffi_add_node_4" ffi_add_node_4 :: Ptr () -> CShort -> IO ()
foreign import ccall safe "ffi_add_node_6" ffi_add_node_6 :: Ptr () -> CShort -> IO ()

foreign import ccall safe "ffi_load_bootstrap_nodes" ffi_load_bootstrap :: CString -> IO CInt
foreign import ccall safe "ffi_save_bootstrap_nodes" ffi_save_bootstrap :: CString -> IO CInt

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

data DHT = DHT {
        searches :: MVar (Map.Map DHTID (TChan SearchResult, Int)),
        callback :: MVar (FunPtr FFICallback),
        protoCount :: Int,
        announcePort :: CShort
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

data DHTException = DHTFail Int
                  | BootstrapLoad Int
                  | BootstrapSave Int
    deriving (Show, Typeable)

instance Exception DHTException

-- |Runs the DHT and returns an error code (0 means everything's ok)
-- Return an exception from an error code
runDHT :: DHT
       -> Maybe HostAddress     -- ^ IPv4 address
       -> Maybe HostAddress6    -- ^ IPv6 address
       -> Int                   -- ^ Port number
       -> DHTID                 -- ^ DHT ID
       -> IO ()                 -- ^ Return DHT structure (use exception?)
runDHT dht v4 v6 port dht_id = runResourceT $ do
    dhtid <- liftIO malloc
    liftIO $ poke dhtid dht_id
    (_, (_, fd4)) <- allocate (liftIO $ makeSocket4 port v4) closeSock
    (_, (_, fd6)) <- allocate (liftIO $ makeSocket6 port v6) closeSock
    callback <- liftIO $ readMVar (callback dht)
    r <- liftIO $ ffi_run_dht fd4 fd6 (castPtr dhtid) callback

    -- check r for exceptions
    when (fromIntegral r /= 0) $ throw . DHTFail $ fromIntegral r
    where
        closeSock = maybe (return ()) close . fst

makeDHT count port = do
    entries <- newMVar Map.empty
    cb <- newEmptyMVar
    let dht = DHT {
            searches = entries,
            callback = cb,
            protoCount = count,
            announcePort = fromIntegral port
        }
        in do
        mkCallback (ffiCallback dht) >>= putMVar (callback dht)
        return dht

startDHT :: Maybe HostAddress     -- ^ IPv4 address
         -> Maybe HostAddress6    -- ^ IPv6 address
         -> Int                   -- ^ Port number
         -> DHTID                 -- ^ DHT ID
         -> IO DHT                -- ^ Return DHT structure (use exception?)
startDHT v4 v6 port dht_id = do
    dht <- makeDHT (fromEnum (isJust v4) + fromEnum (isJust v6)) port
    forkIO $ runDHT dht v4 v6 (fromIntegral port) dht_id `onException` stopDHT dht
    return dht

-- |Stop the DHT and clear all channels on the DHT side
stopDHT :: DHT -> IO ()
stopDHT dht = do
    ffi_stop_dht
    modifyMVar_ (searches dht) (\_ -> pure Map.empty) -- zero the map of searches
    cb <- tryTakeMVar (callback dht)
    return $ freeHaskellFunPtr <$> cb
    return ()

-- |Utility functions to create a socket IPv4
makeSocket4 :: Int -> Maybe HostAddress -> IO (Maybe Socket, CInt)
makeSocket4 port = maybe nullSocket (\host -> do
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock (SockAddrInet (fromIntegral port) host) `onException`
        close sock >> nullSocket
    return (Just sock, fdSocket sock))

-- |Utility functions to create a socket IPv6
makeSocket6 :: Int -> Maybe HostAddress6 -> IO (Maybe Socket, CInt)
makeSocket6 port = maybe nullSocket (\host -> do
    sock <- socket AF_INET6 Datagram defaultProtocol
    bind sock (SockAddrInet6 (fromIntegral port) 0 host 0) `onException`
        close sock >> nullSocket
    return (Just sock, fdSocket sock))

nullSocket :: (Monad m) => m (Maybe Socket, CInt)
nullSocket = return (Nothing, CInt $! negate 1)

-- |Generates a random DHTID
generateID :: IO DHTID
generateID = do
    a1 <- randomIO
    a2 <- randomIO
    a3 <- randomIO
    return $ DHTID a1 a2 a3

-- |Searches for the specific DHT ID
search :: DHT       -- ^ DHT Instance
       -> DHTID     -- ^ DHT ID to look for
       -> IO (TChan SearchResult) -- ^ new DHT structure and the channel to listen to
search dht dst = search' dht dst 0

-- |Searches for the specific DHT ID
announce :: DHT     -- ^ DHT Instance
       -> DHTID     -- ^ DHT ID to look for
       -> IO (TChan SearchResult) -- ^ new DHT structure and the channel to listen to
announce dht dst = search' dht dst (announcePort dht)

search' :: DHT      -- ^ DHT Instance
       -> DHTID     -- ^ DHT ID to look for
       -> CShort    -- ^ port
       -> IO (TChan SearchResult) -- ^ new DHT structure and the channel to listen to
search' dht dht_id port = do
    tchan <- newTChanIO
    forkIO $ do
        dhtid <- malloc
        poke dhtid dht_id
        readMVar (callback dht) >>= ffi_search (castPtr dhtid) port
    modifyMVar_ (searches dht) $ pure . Map.insert dht_id (tchan, protoCount dht)
    return tchan

-- |Callback from DHT
ffiCallback :: DHT -> FFICallback
ffiCallback dht _ event hash addr len = do
    dht_id <- peek (castPtr hash)-- :: IO DHTID
    ffiCallback' dht event dht_id addr len

ffiCallback' dht event dht_id addr len = do
    mchan <- withMVar (searches dht) $ return . Map.lookup dht_id
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
                                    modifyMVar_ (searches dht) (pure . Map.delete dht_id)

        decrease dht = let
            decreaseElem = maybe Nothing (\(t,c) -> Just (t,c-1))
            in modifyMVar_ (searches dht) (pure . Map.alter decreaseElem dht_id)

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
addNode (IPv4 ip) port = flip addHostAddress port . toHostAddress $ ip
addNode (IPv6 ip) port = flip addHostAddress6 port . toHostAddress6 $ ip

addHostAddress addr port =
    allocaArray 4 (\ipPtr -> do
        poke ipPtr addr
        ffi_add_node_4 (castPtr ipPtr) $ fromIntegral port)
addHostAddress6 addr port =
    allocaArray 16 (\ipPtr -> do
        poke ipPtr addr
        ffi_add_node_6 (castPtr ipPtr) $ fromIntegral port)

bootstrapNodes = [
    ("router.bittorrent.com", 8991),
    ("router.utorrent.com", 6881),
    ("dht.transmissionbt.com", 6881)]

addRemoteBootstrapNodes :: IO ()
addRemoteBootstrapNodes = do
    addresses <- mapM getBootstrapNodes bootstrapNodes
    mapM_ (uncurry addSockAddr) $ Prelude.concat addresses
    where
        getBootstrapNodes (host,port) =
            getAddrInfo Nothing (Just host) Nothing >>=
            mapM (pure . \a -> (addrAddress a,port))

        addSockAddr (SockAddrInet _ addr) = addHostAddress addr
        addSockAddr (SockAddrInet6 _ addr _ _) = addHostAddress6 addr

loadBootstrap :: String -> IO Int
loadBootstrap path = do
    r <- fromIntegral <$> withCString path ffi_load_bootstrap
    when (fromIntegral r < 0) . throw $
        mkIOError userErrorType (show r++" Loading DHT bootstrap data") Nothing (Just path)
    return r

saveBootstrap :: String -> IO Int
saveBootstrap path = do
    r <- fromIntegral <$> withCString path ffi_save_bootstrap
    when (fromIntegral r < 0) . throw $
        mkIOError userErrorType (show r++" Saving DHT bootstrap data") Nothing (Just path)
    return r

conditionalBootstrap :: String -> IO Int
conditionalBootstrap file = do
    debugM logModule $ "Check bootstrap file "++file
    exists <- doesFileExist file
    debugM logModule $ "Exists? "++show exists
    if exists
    then do
        debugM logModule "Loading bootstrap nodes from file"
        n <- loadBootstrap "nodes.dump"
        when (n < 100) addRemoteBootstrapNodes
        debugM logModule $ "found " ++ show n ++ " nodes"
        return n
    else addRemoteBootstrapNodes >> return 0

