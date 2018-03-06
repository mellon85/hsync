{-# LANGUAGE BangPatterns #-}
module HashUtils (
        chunksumC, AdlerHash, ChunkedSum(..), sinkSeq, max_size, min_size
    ) where

import Data.Conduit
import Data.Void
import Data.Monoid (mempty)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import Data.Sequence
import Data.Maybe
import Data.Bits

import System.IO

bit_mask :: Word32
bit_mask = 0x7FFF

window_size :: Word32
window_size = 8192

min_size :: Word32
min_size = 512*1024

max_size :: Word32
max_size = 1024*1024*2

type Adler32 = (Word32, Word32)
type AdlerHash = Word32

adler_mod :: Word32
adler_mod = 65521

adlerInit :: Adler32
adlerInit = (1, 0)

adlerHash :: Adler32 -> Word32
adlerHash (!a, !b) = b * 0xFF + a

adlerAdd :: Adler32 -> Word8 -> Adler32
{-# INLINE adlerAdd #-}
adlerAdd (!a, !b) !byte = let a' = a + (fromIntegral byte) `mod` adler_mod
                              b' = a' + b `mod` adler_mod
                              in (a', b')

adlerShift :: Adler32 -> Word8 -> Word8 -> Adler32
{-# INLINE adlerShift #-}
adlerShift (!a, !b) !byteIn !byteOut =
    let bOut = fromIntegral byteOut
        bIn = fromIntegral byteIn
        a' = (a + bIn - bOut) `mod` adler_mod
        b' = (a' + b - bOut * (fromIntegral $ window_size-1)) `mod` adler_mod
        in (a', b')

data ChunkedSum = CS {
        hash :: Word32,
        chunkSize :: Word32
    } deriving (Show, Eq)

instance Storable ChunkedSum where
    sizeOf _ = 4 + 4 -- length + adler hash
    alignment _ = 8
    peek ptr = do
        hash <- peek $ castPtr ptr
        len  <- peekElemOff (castPtr ptr) (sizeOf hash)
        return $! CS hash len
    poke ptr (CS hash len) = do
        poke (castPtr ptr) hash
        pokeElemOff (castPtr ptr) (sizeOf hash) len


type ProduceChunkedSum m = ConduitM () ChunkedSum m ()

chunksumC :: (MonadIO m) => Handle -> ProduceChunkedSum m
chunksumC handle = do
    lazy_data <- liftIO $ BL.hGetContents handle
    tailSum lazy_data adlerInit adlerInit 0 lazy_data
  where
    tailSum :: (Monad m) => BL.ByteString
                         -> Adler32 -> Adler32
                         -> Word32 -> BL.ByteString
                         -> ProduceChunkedSum m
    tailSum bs !current !total !count !out =
        case BL.uncons bs of
            Nothing          -> do
                yield $! CS (adlerHash current) count
                yield $! CS (adlerHash total) 0
            Just (byte, bs') ->
                if chunkPoint current count
                then do yield $! CS (adlerHash current) count
                        tailSum bs' adlerInit adlerInit 0 bs'
                else consumeByte current total count out byte (tailSum bs')

    consumeByte !curr !total !cnt !buf !byte cont
        | cnt < window_size = do
          cont (adlerAdd curr byte)
               (adlerAdd total byte)
               (cnt + 1)
               buf
        | otherwise =
          let (rest', bs') = (BL.head buf, BL.tail buf) in
          cont (adlerShift curr byte rest')
               (adlerAdd total byte)
               (cnt + 1)
               bs'

    chunkPoint :: Adler32 -> Word32 -> Bool
    chunkPoint current count
        | (adlerHash current) .&. bit_mask == 0 && count >= min_size = True
        | count > max_size = True
        | otherwise = False

-- reads all checksums and returns a tuple with the total checksum in the first
-- place and the sequence of checksums as the second
sinkSeq :: Monad m => ConduitM ChunkedSum Void m (AdlerHash, Seq ChunkedSum)
sinkSeq = loop mempty
    where
        loop !s = do
            Just v <- await
            if chunkSize v == 0
            then return $ (hash v, s)
            else loop (s |> v)
