module Fathens.Bitcoin.Binary.Num (
  Word256(..)
, BigEndianFixed(..)
, toWord256
, fromBigEndian
, toBigEndian
, putBigEndianFixed
) where

import           Control.Monad
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Word            (Word32)
import           GHC.Enum
import           GHC.Real
import           System.Random

-- Constants

bitsWord256 = 256

-- Data

data Word256 = Word256 Integer deriving (Show, Eq)

instance Ord Word256 where
  (<=) (Word256 a) (Word256 b) = a <= b

instance Num Word256 where
  (+) (Word256 a) (Word256 b) = Word256 (a + b)
  (*) (Word256 a) (Word256 b) = Word256 (a * b)
  negate (Word256 a) = Word256 (-a)
  abs (Word256 a) = Word256 (abs a)
  signum (Word256 0) = 0
  signum (Word256 _) = 1
  fromInteger i = Word256 i

instance Bounded Word256 where
  minBound = 0
  maxBound = 2 ^ bitsWord256 - 1

instance Enum Word256 where
  succ a
    | a < maxBound = a + 1
    | otherwise = succError "Word256"
  pred a
    | a > minBound = a - 1
    | otherwise = predError "Word256"
  toEnum i
    | minBound <= i && i <= maxBound = Word256 $ fromIntegral i
    | otherwise = toEnumError "Word256" i (minBound, maxBound :: Word256)
  fromEnum a@(Word256 i)
    | i <= fromIntegral (maxBound :: Int) = fromIntegral i
    | otherwise = fromEnumError "Word256" a

instance Real Word256 where
  toRational (Word256 i) = toRational i

instance Integral Word256 where
  quotRem (Word256 a) (Word256 b) = let (i, j) = quotRem a b
                                    in (Word256 i, Word256 j)
  toInteger (Word256 i) = i

instance Bits Word256 where
  isSigned _ = False
  bitSize = finiteBitSize
  bitSizeMaybe = Just . finiteBitSize
  shift (Word256 a) = Word256 . shift a
  rotate (Word256 a) = Word256 . rotate a
  (.&.) (Word256 a) (Word256 b) = Word256 (a .&. b)
  (.|.) (Word256 a) (Word256 b) = Word256 (a .|. b)
  xor (Word256 a) (Word256 b) = Word256 (a `xor` b)
  complement (Word256 a) = let (Word256 b) = maxBound
                           in Word256 (a `xor` b)
  popCount = popCountDefault
  testBit = testBitDefault
  bit = bitDefault

instance FiniteBits Word256 where
  finiteBitSize _ = fromIntegral bitsWord256

instance Random Word256 where
  randomR ((Word256 a), (Word256 b)) g = let (x, y) = randomR (a, b) g
                                         in (Word256 x, y)
  random g = let (x, y) = random g
             in (Word256 x, y)

instance BigEndianFixed Word256 where
  lengthOfBytes = bitsWord256 `div` 8

  decodeBigEndian bs = do
    let (d, o) = BS.splitAt (fromIntegral (lengthOfBytes :: Word256)) bs
    v <- fromBigEndianFixed d
    return (v, o)

  fromBigEndianFixed bs = do
    guard $ BS.length bs == fromIntegral (lengthOfBytes :: Word256)
    return $ fromIntegral $ fromBigEndian bs

  toBigEndianFixed (Word256 i) = putBigEndianFixed (lengthOfBytes :: Word256) i

instance BigEndianFixed Word32 where
  lengthOfBytes = 32 `div` 8

  decodeBigEndian bs = do
    let (d, o) = BS.splitAt (fromIntegral (lengthOfBytes :: Word32)) bs
    v <- fromBigEndianFixed d
    return (v, o)

  fromBigEndianFixed bs = do
    guard $ BS.length bs == fromIntegral (lengthOfBytes :: Word32)
    return $ fromIntegral $ fromBigEndian bs

  toBigEndianFixed i = putBigEndianFixed (lengthOfBytes :: Word32) $ toInteger i

-- Classes

class FiniteBits a => BigEndianFixed a where
  lengthOfBytes :: a
  decodeBigEndian :: ByteString -> Maybe (a, ByteString)
  fromBigEndianFixed :: ByteString -> Maybe a
  toBigEndianFixed :: a -> ByteString

-- Functions

toWord256 :: Integer -> Maybe Word256
toWord256 i = do
  guard $ min <= i && i <= max
  return $ Word256 i
  where
    min = toInteger (minBound :: Word256)
    max = toInteger (maxBound :: Word256)

fromBigEndian :: ByteString -> Integer
fromBigEndian = BS.foldr f 0 . BS.reverse
  where
    f v i = shiftL i 8 .|. fromIntegral v

toBigEndian :: Integer -> ByteString
toBigEndian = BS.reverse . BS.unfoldr f
  where
    f 0 = Nothing
    f i = Just (fromInteger i, shiftR i 8)

putBigEndianFixed :: (Integral n) => n -> Integer -> ByteString
putBigEndianFixed n = padLeft . toBigEndian
  where
    len = fromIntegral n
    padLeft d = BS.replicate (len - BS.length d') 0 `BS.append` d'
      where
        d' = BS.take len d
