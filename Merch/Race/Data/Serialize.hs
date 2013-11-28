
module Merch.Race.Data.Serialize
  ( Serialize(..)
  , hPutConvert
  , hGetConvert
  ) where

import Control.Monad
import Data.Bits
import Data.Int
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Ratio
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO

class Serialize a where
  hPut :: Handle -> a -> IO ()
  hGet :: Handle -> IO a

{- Although we *could* use Storable instances directly,
   doing that leaves us to the mercy of the target machine's
   endianness.  By splitting up words "manually" without
   the help of Storable, we can mandate the ONE TRUE WAY to
   correctly store data, which is, of course, as everybody
   knows, little-endian.  -}
instance Serialize Word8 where
  hPut h w = alloca $ \p -> do
    poke p w
    hPutBuf h p 1
  hGet h = alloca $ \p -> do
    hGetBuf h p 1
    peek p
instance Serialize Word16 where
  hPut h w = allocaBytes 2 $ \p -> do
    let bs = [ fromIntegral $ (w `shiftR` 0) .&. 255
             , fromIntegral $ (w `shiftR` 8) .&. 255
             ]
    forM_ (zip [0..1] bs) $ \ (n, b) -> do
      pokeByteOff p n (b :: Word8)
    hPutBuf h p 2
  hGet h = allocaBytes 2 $ \p -> do
    hGetBuf h p 2
    [b0, b1] <- forM [0..1] $ \n -> do
      peekByteOff p n :: IO Word8
    return $ ((fromIntegral b0) `shiftL` 0)
         .|. ((fromIntegral b1) `shiftL` 8)
instance Serialize Word32 where
  hPut h w = allocaBytes 2 $ \p -> do
    let bs = [ fromIntegral $ (w `shiftR`  0) .&. 255
             , fromIntegral $ (w `shiftR`  8) .&. 255
             , fromIntegral $ (w `shiftR` 16) .&. 255
             , fromIntegral $ (w `shiftR` 24) .&. 255
             ]
    forM_ (zip [0..3] bs) $ \ (n, b) -> do
      pokeByteOff p n (b :: Word8)
    hPutBuf h p 4
  hGet h = allocaBytes 2 $ \p -> do
    hGetBuf h p 4
    [b0, b1, b2, b3] <- forM [0..3] $ \n -> do
      peekByteOff p n :: IO Word8
    return $ ((fromIntegral b0) `shiftL`  0)
         .|. ((fromIntegral b1) `shiftL`  8)
         .|. ((fromIntegral b2) `shiftL` 16)
         .|. ((fromIntegral b3) `shiftL` 24)
instance Serialize Word64 where
  hPut h w = allocaBytes 2 $ \p -> do
    let bs = [ fromIntegral $ (w `shiftR`  0) .&. 255
             , fromIntegral $ (w `shiftR`  8) .&. 255
             , fromIntegral $ (w `shiftR` 16) .&. 255
             , fromIntegral $ (w `shiftR` 24) .&. 255
             , fromIntegral $ (w `shiftR` 32) .&. 255
             , fromIntegral $ (w `shiftR` 40) .&. 255
             , fromIntegral $ (w `shiftR` 48) .&. 255
             , fromIntegral $ (w `shiftR` 56) .&. 255
             ]
    forM_ (zip [0..7] bs) $ \ (n, b) -> do
      pokeByteOff p n (b :: Word8)
    hPutBuf h p 8
  hGet h = allocaBytes 2 $ \p -> do
    hGetBuf h p 8
    [b0, b1, b2, b3, b4, b5, b6, b7] <- forM [0..7] $ \n -> do
      peekByteOff p n :: IO Word8
    return $ ((fromIntegral b0) `shiftL`  0)
         .|. ((fromIntegral b1) `shiftL`  8)
         .|. ((fromIntegral b2) `shiftL` 16)
         .|. ((fromIntegral b3) `shiftL` 24)
         .|. ((fromIntegral b4) `shiftL` 32)
         .|. ((fromIntegral b5) `shiftL` 40)
         .|. ((fromIntegral b6) `shiftL` 48)
         .|. ((fromIntegral b7) `shiftL` 56)

-- For easy implementation of Serialize instances of
-- data types which can be trivially converted to
-- simpler Serialize data types.
hPutConvert :: Serialize basetype => (a -> basetype) -> Handle -> a -> IO ()
hPutConvert convert h a = hPut h (convert a)
hGetConvert :: Serialize basetype => (basetype -> a) -> Handle -> IO a
hGetConvert convert h = hGet h >>= return . convert

-- We assume that Word fits into Word32.
instance Serialize Word where
  hPut = hPutConvert (fromIntegral :: Word   -> Word32)
  hGet = hGetConvert (fromIntegral :: Word32 -> Word  )
-- We assume that Int types are trivially convertible
-- to the corresponding Word types, and that the
-- conversion back to Int will restore negative signs.
instance Serialize Int where
  hPut = hPutConvert (fromIntegral :: Int    -> Word  )
  hGet = hGetConvert (fromIntegral :: Word   -> Int   )
instance Serialize Int8 where
  hPut = hPutConvert (fromIntegral :: Int8   -> Word8 )
  hGet = hGetConvert (fromIntegral :: Word8  -> Int8  )
instance Serialize Int16 where
  hPut = hPutConvert (fromIntegral :: Int16  -> Word16)
  hGet = hGetConvert (fromIntegral :: Word16 -> Int16 )
instance Serialize Int32 where
  hPut = hPutConvert (fromIntegral :: Int32  -> Word32)
  hGet = hGetConvert (fromIntegral :: Word32 -> Int32 )
instance Serialize Int64 where
  hPut = hPutConvert (fromIntegral :: Int64  -> Word64)
  hGet = hGetConvert (fromIntegral :: Word64 -> Int64 )

-- Serializing common builtin datatypes
instance Serialize Bool where
  hPut h t = hPut h (if t then 1 else 0 :: Word8)
  hGet h = (hGet h :: IO Word8)
       >>= \i -> return $ if i == 0 then False else True
instance Serialize a => Serialize (Maybe a) where
  hPut h Nothing = do
    hPut h (0 :: Word8)
  hPut h (Just a) = do
    hPut h (1 :: Word8)
    hPut h a
  hGet h = do
    i <- hGet h :: IO Word8
    if i == 0
     then return Nothing
     else hGet h >>= return . Just
instance (Serialize a, Serialize b) => Serialize (Either a b) where
  hPut h (Left a)  = do
    hPut h (0 :: Word8)
    hPut h a
  hPut h (Right b) = do
    hPut h (1 :: Word8)
    hPut h b
  hGet h = do
    i <- hGet h :: IO Word8
    if i == 0
     then hGet h >>= return . Left
     else hGet h >>= return . Right
instance Serialize a => Serialize [a] where
  hPut h as = do
    hPut h (length as :: Int)
    forM_ as $ hPut h
  hGet h = do
    n <- hGet h :: IO Int
    forM [1..n] $ \_ -> hGet h
-- important data structures in base.
instance (Serialize a, Serialize b, Ord a) => Serialize (Map a b) where
  hPut h s = hPut h $ Map.toList s
  hGet h = hGet h >>= return . Map.fromList
instance (Serialize a, Integral a) => Serialize (Ratio a) where
  hPut h x = do
    let n = numerator x
        d = denominator x
    hPut h n
    hPut h d
  hGet h = do
    n <- hGet h
    d <- hGet h
    return $ n % d
instance (Serialize a, Ord a) => Serialize (Set a) where
  hPut h s = hPut h $ Set.toList s
  hGet h = hGet h >>= return . Set.fromList

{- Integer serialization.
   We observe that most Integers are small, fitting in
   one or two bytes.  So, we use the following format:
   1.  The first byte is either 0 to indicate nothing
       follows and the number is a 0, or a signed
       number of bytes (the absolute value is the
       number of bytes, the sign is the sign of the
       Integer).
   2.  If the signed number of bytes has magnitude
       127, it means that the integer is so freaking
       huge that it takes 127 or more bytes to store it.
       If so, this means there's a serialized Word
       after the first byte, which contains the real
       number of bytes in the Integer.
   3.  The Integer's magnitude is stored, lowest byte
       first, in the succeeding space.  The sign is the
       sign of the first byte.  -}
-- split/unsplit Integers into bytes
blast :: Integer -> (Bool, [Word8])
blast i
  | i < 0     = (True,  loop $ negate i)
  | otherwise = (False, loop i)
 where
  loop i
    | i == 0    = []
    | otherwise = let (q, r) = i `divMod` 256
                  in fromIntegral r:loop q
unblast :: (Bool, [Word8]) -> Integer
unblast (s, ws)
  | s         = negate $ loop ws
  | otherwise = loop ws
 where
  loop = core 0 1
  core acc mult []     = acc
  core acc mult (w:ws) = core (acc + fromIntegral w * mult) (mult * 256) ws
instance Serialize Integer where
  hPut h i = do
    let (s, ws) = blast i
        num = genericLength ws :: Word
        sign
          | s         = negate
          | otherwise = id
    if num <= 126
     then do
       hPut h (sign $ fromIntegral num :: Int8 )
     else do
       hPut h (sign 127 :: Int8)
       hPut h (num :: Word)
    forM_ ws $ hPut h
  hGet h = do
   nb <- hGet h :: IO Int8
   let s = nb < 0
       sign
         | s         = negate
         | otherwise = id
       num
         | s         = negate nb
         | otherwise = nb
   num <- if num <= 126
    then return $ fromIntegral num
    else hGet h :: IO Word
   ws <- forM [1..num] $ \_ -> hGet h
   return $ unblast (s, ws)

{- Encode Char using UTF-8.  -}
instance Serialize Char where
  hPut h c = do
    case fromEnum c of
      i | i <=     0x007F -> hPut h (fromIntegral i :: Word8)
        | i <=     0x07FF -> encode2 h i
        | i <=     0xFFFF -> encode3 h i
        | i <=   0x1FFFFF -> encode4 h i
        | i <=  0x3FFFFFF -> encode5 h i
        | i <= 0x7FFFFFFF -> encode6 h i
        | otherwise       -> fail "Character too large to write."
   where
    encode2 h i = do
      let (i0, i1) =
            ( (i `shiftR` 6)
            , (i `shiftR` 0) .&. 0x3F
            )
          ws = [ 0xC0 .|. fromIntegral i0
               , 0x80 .|. fromIntegral i1
               ] :: [Word8]
      forM_ ws $ hPut h
    encode3 h i = do
      let (i0, i1, i2) =
            ( (i `shiftR` 12)
            , (i `shiftR`  6) .&. 0x3F
            , (i `shiftR`  0) .&. 0x3F
            )
          ws = [ 0xE0 .|. fromIntegral i0
               , 0x80 .|. fromIntegral i1
               , 0x80 .|. fromIntegral i2
               ] :: [Word8]
      forM_ ws $ hPut h
    encode4 h i = do
      let (i0, i1, i2, i3) =
            ( (i `shiftR` 18)
            , (i `shiftR` 12) .&. 0x3F
            , (i `shiftR`  6) .&. 0x3F
            , (i `shiftR`  0) .&. 0x3F
            )
          ws = [ 0xF0 .|. fromIntegral i0
               , 0x80 .|. fromIntegral i1
               , 0x80 .|. fromIntegral i2
               , 0x80 .|. fromIntegral i3
               ] :: [Word8]
      forM_ ws $ hPut h
    encode5 h i = do
      let (i0, i1, i2, i3, i4) =
            ( (i `shiftR` 24)
            , (i `shiftR` 18) .&. 0x3F
            , (i `shiftR` 12) .&. 0x3F
            , (i `shiftR`  6) .&. 0x3F
            , (i `shiftR`  0) .&. 0x3F
            )
          ws = [ 0xF0 .|. fromIntegral i0
               , 0x80 .|. fromIntegral i1
               , 0x80 .|. fromIntegral i2
               , 0x80 .|. fromIntegral i3
               , 0x80 .|. fromIntegral i4
               ] :: [Word8]
      forM_ ws $ hPut h
    encode6 h i = do
      let (i0, i1, i2, i3, i4, i5) =
            ( (i `shiftR` 30)
            , (i `shiftR` 24) .&. 0x3F
            , (i `shiftR` 18) .&. 0x3F
            , (i `shiftR` 12) .&. 0x3F
            , (i `shiftR`  6) .&. 0x3F
            , (i `shiftR`  0) .&. 0x3F
            )
          ws = [ 0xF0 .|. fromIntegral i0
               , 0x80 .|. fromIntegral i1
               , 0x80 .|. fromIntegral i2
               , 0x80 .|. fromIntegral i3
               , 0x80 .|. fromIntegral i4
               , 0x80 .|. fromIntegral i5
               ] :: [Word8]
      forM_ ws $ hPut h
  hGet h = do
    i <- hGet h :: IO Word8
    case i of
      i | (i .&. 0x80) == 0x00 -> return $ toEnum $ fromIntegral i
        | (i .&. 0xE0) == 0xC0 -> decode2 h (i .&. 0x1F)
        | (i .&. 0xF0) == 0xE0 -> decode3 h (i .&. 0x0F)
        | (i .&. 0xF8) == 0xF0 -> decode4 h (i .&. 0x07)
        | (i .&. 0xFC) == 0xF8 -> decode5 h (i .&. 0x03)
        | (i .&. 0xFE) == 0xFC -> decode6 h (i .&. 0x01)
        | otherwise            -> fail "Invalid UTF-8 encoding."
   where
    readCont h = do
      w <- hGet h :: IO Word8
      when ((w .&. 0xC0) /= 0x80) $ do
        fail "Expecting continuation character in UTF-8 encoding."
      return $ w .&. 0x3F
    decode2 h w0 = do
      [w1] <- forM [2..2] $ \_ -> readCont h
      return $ toEnum
             $ (fromIntegral w0 `shiftL` 6)
           .|. (fromIntegral w1 `shiftL` 0)
    decode3 h w0 = do
      [w1, w2] <- forM [2..3] $ \_ -> readCont h
      return $ toEnum
             $ (fromIntegral w0 `shiftL` 12)
           .|. (fromIntegral w1 `shiftL`  6)
           .|. (fromIntegral w2 `shiftL`  0)
    decode4 h w0 = do
      [w1, w2, w3] <- forM [2..4] $ \_ -> readCont h
      return $ toEnum
             $ (fromIntegral w0 `shiftL` 18)
           .|. (fromIntegral w1 `shiftL` 12)
           .|. (fromIntegral w2 `shiftL`  6)
           .|. (fromIntegral w3 `shiftL`  0)
    decode5 h w0 = do
      [w1, w2, w3, w4] <- forM [2..5] $ \_ -> readCont h
      return $ toEnum
             $ (fromIntegral w0 `shiftL` 24)
           .|. (fromIntegral w1 `shiftL` 18)
           .|. (fromIntegral w2 `shiftL` 12)
           .|. (fromIntegral w3 `shiftL`  6)
           .|. (fromIntegral w4 `shiftL`  0)
    decode6 h w0 = do
      [w1, w2, w3, w4, w5] <- forM [2..6] $ \_ -> readCont h
      return $ toEnum
             $ (fromIntegral w0 `shiftL` 30)
           .|. (fromIntegral w1 `shiftL` 24)
           .|. (fromIntegral w2 `shiftL` 18)
           .|. (fromIntegral w3 `shiftL` 12)
           .|. (fromIntegral w4 `shiftL`  6)
           .|. (fromIntegral w5 `shiftL`  0)

-- Freaking Tuples!
instance (Serialize i0, Serialize i1) => Serialize (i0, i1) where
  hPut h (i0,i1) = do
    hPut h i0
    hPut h i1
  hGet h = do
    i0 <- hGet h
    i1 <- hGet h
    return (i0,i1)
instance (Serialize i0, Serialize i1, Serialize i2)
      => Serialize (i0, i1, i2) where
  hPut h (i0,i1,i2) = do
    hPut h i0
    hPut h i1
    hPut h i2
  hGet h = do
    i0 <- hGet h
    i1 <- hGet h
    i2 <- hGet h
    return (i0,i1,i2)
instance (Serialize i0, Serialize i1, Serialize i2, Serialize i3)
      => Serialize (i0, i1, i2, i3) where
  hPut h (i0,i1,i2,i3) = do
    hPut h i0
    hPut h i1
    hPut h i2
    hPut h i3
  hGet h = do
    i0 <- hGet h
    i1 <- hGet h
    i2 <- hGet h
    i3 <- hGet h
    return (i0,i1,i2,i3)
instance (Serialize i0, Serialize i1, Serialize i2, Serialize i3, Serialize i4)
      => Serialize (i0, i1, i2, i3, i4) where
  hPut h (i0,i1,i2,i3,i4) = do
    hPut h i0
    hPut h i1
    hPut h i2
    hPut h i3
    hPut h i4
  hGet h = do
    i0 <- hGet h
    i1 <- hGet h
    i2 <- hGet h
    i3 <- hGet h
    i4 <- hGet h
    return (i0,i1,i2,i3,i4)
-- Freaking Tuples! Ends
