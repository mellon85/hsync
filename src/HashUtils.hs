{-# LANGUAGE BangPatterns, TypeFamilies, FlexibleContexts #-}
module HashUtils (
        chunksumC, AdlerHash, ChunkedSum(..), sinkSeq, max_size, min_size
    )
    where

-- base
import Foreign.Storable
import Foreign.Ptr
import Control.Monad
import Data.Word
import Data.Bits
import Control.Monad.IO.Class
import Data.Sequence

-- vector
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import qualified Data.ByteString as BS

-- conduit
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC



min_size :: Word32
min_size = 512*1024-1

max_size :: Word32
max_size = 8192*1024

window_size :: Word32
window_size = 8*1024

bit_mask :: Word32
bit_mask = 0x7FFFF -- 14 bits

bit_mask32 :: Word32
bit_mask32 = 0x7FFFF -- 14 bits

adler_mod :: Word32
adler_mod = 65521

type Adler32 = (Word32, Word32)
type AdlerHash = Word32

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

data ChunkedSumState = CSS Adler32
                           (Maybe (VU.Vector Word8))
                           BS.ByteString
                           Word32
                           Adler32 -- ^ Total file hash

-- reads all checksums and returns a tuple with the total checksum in the first
-- place and the sequence of checksums as the second
sinkSeq :: Monad m => Consumer ChunkedSum m (AdlerHash, Seq ChunkedSum)
sinkSeq = do
    Just v' <- await
    loop mempty v'
    where
        loop !s !t = do
            v <- await
            case v of
                Nothing -> return $ (hash t, s)
                Just v  -> s `seq` loop (s |> t) v


-- the last entry returned is the total file hash
chunksumC :: MonadIO m => Conduit BS.ByteString m ChunkedSum
chunksumC = chunksum (initialState adlerInit)

initialState :: Adler32 -> ChunkedSumState
initialState total = CSS adlerInit
                        (Just $ VU.replicate (fromIntegral window_size) 0)
                        BS.empty 0 total

chunksum :: MonadIO m => ChunkedSumState -> Conduit BS.ByteString m ChunkedSum
chunksum css_in@(CSS h _ _ cnt total) = do
    chunk_in <- await
    case chunk_in of
        Nothing    -> do
            yield (CS (adlerHash h) cnt)
            yield (CS (adlerHash total) 0)
        Just chunk -> do
            state@(CSS hash window rest count tot) <- liftIO $ step css_in chunk
            case window of
                Just _  -> chunksum state
                Nothing -> do
                    yield (CS (adlerHash hash) count)
                    when (BS.null rest) $ leftover rest
                    chunksum (initialState tot)

    where
        step :: ChunkedSumState -> BS.ByteString -> IO ChunkedSumState
        step (CSS h (Just w) _ cnt tot) chunk = do
            -- prepare
            vec <- VU.unsafeThaw w

            -- inner loop
            (h', cnt', tot', rest, chunked) <- inner_loop vec h tot cnt chunk

            -- close
            vec' <- VU.unsafeFreeze vec
            if chunked
            then return (CSS h' Nothing rest cnt' tot')
            else return (CSS h' (Just vec') rest cnt' tot')

        inner_loop v h tot cnt chunk
            | BS.null chunk || chunkable = return (h, cnt, tot, chunk, chunkable)
            | otherwise     = do
                let !idx   = fromIntegral $! cnt `mod` window_size
                    !byte  = BS.head chunk
                    chunk' = BS.tail chunk
                prev <- VUM.unsafeRead v idx
                VUM.unsafeWrite v idx byte
                let h'   = adlerShift h byte prev
                    tot' = adlerAdd tot byte
                    cnt' = cnt + 1
                -- all other values are forced due to chunkable and BS.null
                tot' `seq` inner_loop v h' tot' cnt' chunk'
            where
                {-# INLINE chunkable #-}
                chunkable = (adlerHash h .&. bit_mask32 == 0 && cnt > min_size)
                                || cnt >= max_size
