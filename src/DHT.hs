{-# LANGUAGE ForeignFunctionInterface #-}

module DHT
    ( someFunc,
      runDHT,
      stopDHT
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

import Control.Monad

-- Network
import Network.Socket

-- Hash
import Crypto.Hash
import Data.ByteArray

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Callback = CString -> CInt -> CString -> CString -> CUInt -> IO ()

type DHT_ID = Ptr CChar

foreign import ccall safe "ffi_run_dht" runDHT_ :: CInt -> CInt -> CShort ->
    DHT_ID -> FunPtr Callback -> CString -> IO CInt
foreign import ccall safe "ffi_stop_dht" stopDHT :: IO ()
foreign import ccall safe "ffi_search" search :: DHT_ID -> IO ()
foreign import ccall safe "ffi_get_nodes" getNodes :: Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall safe "ffi_add_node" addNode :: Ptr () -> CShort -> IO ()

-- Utility for CInt and so on. Convert an integer from any to any type
conv = fromIntegral . toInteger

type DHTHashCall = Ptr CChar -> CInt -> -- Hash Return
                   Ptr CChar -> CInt -> -- Buffer 1
                   Ptr CChar -> CInt -> -- Buffer 2
                   Ptr CChar -> CInt -> IO () -- Buffer 3

-- dht_hash implementation in haskell
foreign export ccall dht_hash :: DHTHashCall

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
        hash_all = hashFinalize . (hashUpdates hashInit) $ [in_1, in_2, in_3]

-- TODO if socket opening fail close all of them
--      on in case of exceptions
runDHT :: (Maybe HostAddress) -> (Maybe HostAddress6) -> Int -> FunPtr Callback -> String -> IO Int
runDHT v4 v6 port callback path = do
    fd4 <- makeSocket v4 port
    withPersistSocket fd4 (\fd4 -> do
        fd6 <- makeSocket6 v6 port
        withPersistSocket fd6 (\fd6 -> do
                ret <- bracket (mallocArray size) free (\id -> do
                    -- fill ID with random data
                    rand <- Prelude.take size <$> randoms <$> getStdGen
                    copyIn id $ zip [0..size] rand
                    withCString path (\cp -> runDHT_ fd4 fd6 portC id callback cp)
                    )
                return $ fromIntegral ret
            )
        )
    where
        portC = CShort $! fromIntegral port
        size = 20

        copyIn :: (Storable a) => Ptr a -> [(Int, a)] -> IO (Ptr a)
        copyIn ptr vals = let
            copy = \p (off,val) -> pokeElemOff p off val >> return p
                in foldM copy ptr vals

makeSocket :: Maybe HostAddress -> Int -> IO (Maybe Socket)
makeSocket Nothing port = return Nothing
makeSocket (Just host) port = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock $ SockAddrInet (fromIntegral port) host
    return $ Just sock

makeSocket6 :: Maybe HostAddress6 -> Int -> IO (Maybe Socket)
makeSocket6 Nothing port = return Nothing
makeSocket6 (Just host) port = do
    sock <- socket AF_INET6 Stream defaultProtocol
    bind sock $ SockAddrInet6 (fromIntegral port) (fromIntegral 0) host (fromIntegral 0)
    return $ Just sock

withPersistSocket :: Maybe Socket -> (CInt -> IO a) -> IO a
withPersistSocket sock f = (f $ maybe (CInt (0-1)) fdSocket sock) `onException`
    (maybe (return ()) close sock)

-- TODO add generateDHTID function and make it public
