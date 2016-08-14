{-# LANGUAGE ForeignFunctionInterface #-}

module Lib
    ( someFunc,
      runDHT,
      stopDHT
    ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Callback = CString -> CInt -> CString -> CString -> CUInt -> IO ()

type DHT_ID = Ptr CChar

foreign import ccall safe "ffi_run_dht" runDHT :: CInt -> CInt -> CShort -> DHT_ID -> FunPtr Callback -> CString -> IO CInt
foreign import ccall safe "ffi_stop_dht" stopDHT :: IO ()
foreign import ccall safe "ffi_search" search :: DHT_ID -> IO ()
foreign import ccall safe "ffi_get_nodes" getNodes :: Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall safe "ffi_add_node" addNode :: Ptr () -> CShort -> IO ()

type DHTHashCall = Ptr CChar -> CInt -> Ptr CChar -> CInt -> Ptr CChar -> CInt -> Ptr CChar -> CInt -> IO ()

foreign export ccall dht_hash :: DHTHashCall

dht_hash :: DHTHashCall
dht_hash _ _ _ _ _ _ _ _ = do
    return ()



