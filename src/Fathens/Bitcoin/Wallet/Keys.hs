{-# LANGUAGE TypeFamilies #-}
module Fathens.Bitcoin.Wallet.Keys (
  PrvKey
, PubKey
, ECKey
, ECPoint
, PublicKey(..)
, PrivateKey(..)
, XPrvKey
, HDPrvKey
, XPubKey
, HDPubKey
, ExtendPublicKey(..)
, ExtendPrivateKey(..)
, HDNode
, HDNodeNormal
, HDNodeHardened
, generizeHDNode
, normalHDNode
, hardenedHDNode
, genericHDNode
, isHardened
, exKeyFromSeed
, toPrvKey
, toPubKey
, toXPrvKey
, toXPubKey
, maxECC_K
, ecKey
, encodeECPoint
, decodeECPoint
) where

import           Control.Monad
import qualified Crypto.ECC                     as ECC
import           Crypto.Error
import qualified Crypto.PubKey.ECC.P256         as P256
import           Crypto.PubKey.ECC.Prim         (pointAdd, pointMul)
import qualified Crypto.PubKey.ECC.Types        as EC
import           Data.Bits
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.ByteString.Lazy           as BS
import qualified Data.ByteString.Lazy.Char8     as C8
import           Data.List
import           Data.Maybe
import           Data.Word                      (Word32, Word8)
import           Fathens.Bitcoin.Binary.Base58
import           Fathens.Bitcoin.Binary.Hash
import           Fathens.Bitcoin.Binary.Num
import           Fathens.Bitcoin.Wallet.Address
import           GHC.Int                        (Int64)

-- Data

maxECC_K :: Word256
maxECC_K = fromInteger $ EC.ecc_n $ EC.common_curve $ curve

  -- PrvKey prefix key isCompressForPublicKey
data PrvKey = PrvKey Prefix ECKey Bool deriving (Show, Eq)

  -- ECKey ecK
newtype ECKey = ECKey Word256 deriving (Show, Eq)

  -- PubKey prefix ecPoint isCompress
data PubKey = PubKey Prefix ECPoint Bool deriving (Show, Eq)

  -- ECPoint x y
data ECPoint = ECPoint Word256 Word256 deriving (Show, Eq)

data XPrvKey = XPrvKey Prefix HDPrvKey deriving (Show, Eq)
data HDPrvKey = HDPrvKey ExtendData ECKey deriving (Show, Eq)

data XPubKey = XPubKey Prefix HDPubKey deriving (Show, Eq)
data HDPubKey = HDPubKey ExtendData ECPoint deriving (Show, Eq)

data ExtendData = ExtendData {
  depth             :: Word8
, parentFingerPrint :: Word32
, node              :: HDNode
, chainCode         :: Word256
} deriving (Show, Eq)

newtype HDNode = HDNode Word32 deriving (Show, Eq)
newtype HDNodeNormal = HDNodeNormal Word32 deriving (Show, Eq)
newtype HDNodeHardened = HDNodeHardened Word32 deriving (Show, Eq)

-- Instances

instance HDNode_ HDNode where
  isHardened (HDNode v) = 0 /= (v .&. flagHardened)
  encodeHDNode (HDNode index) = toBigEndianFixed index
  decodeHDNode bs = do
    (v, o) <- decodeBigEndian bs
    return (HDNode v, o)
  generizeHDNode a = a

instance HDNode_ HDNodeNormal where
  isHardened _ = False
  encodeHDNode = encodeHDNode . generizeHDNode
  decodeHDNode bs = do
    (n@(HDNode v), o) <- decodeHDNode bs
    guard $ not (isHardened n)
    return (HDNodeNormal v, o)
  generizeHDNode (HDNodeNormal index) = HDNode index

instance HDNode_ HDNodeHardened where
  isHardened _ = True
  encodeHDNode = encodeHDNode . generizeHDNode
  decodeHDNode bs = do
    (n@(HDNode v), o) <- decodeHDNode bs
    guard $ isHardened n
    return (HDNodeHardened $ v `xor` flagHardened, o)
  generizeHDNode (HDNodeHardened v) = HDNode $ v .|. flagHardened

instance ReadFromBase58 PrvKey where
  fromBase58 b58 = do
    prefix <- findBySymbol [PrefixPRV, PrefixCPRV] b58
    c <- isCompressing prefix
    d <- decodeBase58Check b58
    payload <- getPayload prefix d
    k <- fromBigEndianFixed payload
    return $ PrvKey prefix (ECKey k) c

instance WriteToBase58 PrvKey where
  toBase58 (PrvKey prefix (ECKey k) c) = enc k
    where
      enc = encodeBase58Check . appendPayload prefix . toBigEndianFixed

instance WriteToBase58 PubKey where
  toBase58 (PubKey prefix ec c) = enc ec
    where
      enc = encodeBase58Check . appendPayload prefix .
            hash160Data . hash160 . encodeECPoint c

instance ReadFromBase58 XPrvKey where
  fromBase58 b58 = do
    prefix <- findBySymbol [PrefixXPRV] b58
    d <- decodeBase58Check b58
    payload <- getPayload prefix d
    (ed, key) <- decodeExtendData payload
    (s, v) <- BS.uncons key
    guard $ s == 0
    k <- ecKey =<< fromBigEndianFixed v
    return $ XPrvKey prefix $ HDPrvKey ed k

instance WriteToBase58 XPrvKey where
  toBase58 (XPrvKey prefix hd) = encodeBase58Check $ appendPayload prefix d
    where
      HDPrvKey ed (ECKey k) = hd
      d = encodeExtendData ed `BS.append` (0 `BS.cons` toBigEndianFixed k)

instance ReadFromBase58 XPubKey where
  fromBase58 b58 = do
    prefix <- findBySymbol [PrefixXPUB] b58
    d <- decodeBase58Check b58
    payload <- getPayload prefix d
    (ed, key) <- decodeExtendData payload
    (_, p) <- decodeECPoint key
    return $ XPubKey prefix $ HDPubKey ed p

instance WriteToBase58 XPubKey where
  toBase58 (XPubKey prefix hd) = encodeBase58Check $ appendPayload prefix d
    where
      HDPubKey ed ecPoint = hd
      d = encodeExtendData ed `BS.append` encodeECPoint True ecPoint

instance PrivateKey PrvKey where
  type PublicKeyType PrvKey = PubKey
  toPublicKey (PrvKey prvPrefix k c) = PubKey p i c
    where
      p = PrefixP2PKH $ isTestnet prvPrefix
      i = toPublicKey k

instance PrivateKey ECKey where
  type PublicKeyType ECKey = ECPoint
  toPublicKey (ECKey k) = toPoint $ k @* eccG

instance PublicKey PubKey where

instance PublicKey ECPoint where

instance ExtendPrivateKey HDPrvKey where
  type InnerPrivateKeyType HDPrvKey = ECKey
  type ExtendPublicKeyType HDPrvKey = HDPubKey

  getPrivateKey (HDPrvKey _ ec) = ec

  toExtendPublicKey (HDPrvKey ed ec) = HDPubKey ed $ toPublicKey ec

  derivePrivateKey (HDPrvKey ed ec) node = HDPrvKey childData childKey
    where
      ECKey k = ec
      ExtendData depth _ _ chain = ed

      childData = ExtendData (depth + 1) fingerprint
                  (generizeHDNode node) childChain
      childKey = ECKey $ fromInteger $
        ((toInteger k) + (toInteger l)) `mod` (toInteger maxECC_K)

      fingerprint = mkFingerprint $ toPublicKey ec

      (l, childChain) = hdHash chain node body
      body | isHardened node = 0 `BS.cons` toBigEndianFixed k
           | otherwise = encodeECPoint True $ toPublicKey ec

instance ExtendPrivateKey XPrvKey where
  type InnerPrivateKeyType XPrvKey = PrvKey
  type ExtendPublicKeyType XPrvKey = XPubKey

  getPrivateKey (XPrvKey myPrefix (HDPrvKey _ ec)) = PrvKey prefix ec True
    where
      prefix = PrefixCPRV $ isTestnet myPrefix

  toExtendPublicKey (XPrvKey myPrefix hd) = XPubKey prefix child
    where
      prefix = PrefixXPUB $ isTestnet myPrefix
      child = toExtendPublicKey hd

  derivePrivateKey (XPrvKey prefix hd) node = XPrvKey prefix child
    where
      child = derivePrivateKey hd node

instance ExtendPublicKey HDPubKey where
  type InnerPublicKeyType HDPubKey = ECPoint

  getPublicKey (HDPubKey _ p) = p

  derivePublicKey (HDPubKey ed ec) node
    = HDPubKey childData childPoint
    where
      ExtendData depth _ _ chain = ed

      childData = ExtendData (depth + 1) fingerprint
                  (generizeHDNode node) childChain
      childPoint = toPoint $ l @* eccG @+ fromPoint ec

      fingerprint = mkFingerprint ec

      (l, childChain) = hdHash chain node $ encodeECPoint True ec

instance ExtendPublicKey XPubKey where
  type InnerPublicKeyType XPubKey = PubKey

  getPublicKey (XPubKey myPrefix (HDPubKey _ ec)) = PubKey prefix ec True
    where
      prefix = PrefixP2PKH $ isTestnet myPrefix

  derivePublicKey (XPubKey prefix hd) node = XPubKey prefix child
    where
      child = derivePublicKey hd node

hdHash :: HDNode_ n => Word256 -> n -> ByteString -> (Word256, Word256)
hdHash chain node body = (l, r)
  where
    (l, r) = fromJust $ splitHalf $ hash512Data $
      hmac512 (toBigEndianFixed chain) $ body `BS.append` encodeHDNode node

-- Classes

class PrivateKey a where
  type PublicKeyType a :: *
  toPublicKey :: a -> (PublicKeyType a)

class PublicKey a where

class ExtendPrivateKey a where
  type InnerPrivateKeyType a :: *
  type ExtendPublicKeyType a :: *
  getPrivateKey :: a -> (InnerPrivateKeyType a)
  toExtendPublicKey :: a -> (ExtendPublicKeyType a)
  derivePrivateKey :: HDNode_ n => a -> n -> a

class ExtendPublicKey a where
  type InnerPublicKeyType a :: *
  getPublicKey :: a -> (InnerPublicKeyType a)
  derivePublicKey :: a -> HDNodeNormal -> a

class HDNode_ a where
  isHardened :: a -> Bool
  encodeHDNode :: a -> ByteString
  decodeHDNode :: ByteString -> Maybe (a, ByteString)
  generizeHDNode :: a -> HDNode

-- Functions

toPrvKey :: Bool -> Bool -> ECKey -> PrvKey
toPrvKey isTest isCompress k | isCompress = PrvKey (PrefixCPRV isTest) k True
                             | otherwise = PrvKey (PrefixPRV isTest) k False

toPubKey :: Bool -> Bool -> ECPoint -> PubKey
toPubKey isTest isCompress p = PubKey (PrefixP2PKH isTest) p isCompress

toXPrvKey :: Bool -> HDPrvKey -> XPrvKey
toXPrvKey isTest prv = XPrvKey (PrefixXPRV isTest) prv

toXPubKey :: Bool -> HDPubKey -> XPubKey
toXPubKey isTest prv = XPubKey (PrefixXPUB isTest) prv

exKeyFromSeed :: Integer -> Maybe HDPrvKey
exKeyFromSeed i | i < 2^128 = mkMaster 128
                | i < 2^256 = mkMaster 256
                | i < 2^512 = mkMaster 512
                | otherwise = Nothing
  where
    mkMaster n = HDPrvKey d <$> ecKey l
      where
        d = ExtendData 0 0 (HDNode 0) r
        (l, r) = fromJust $ splitHalf $ hash512Data $ hmac512 masterSeed $
                 putBigEndianFixed (n `div` 8) i

ecKey :: Word256 -> Maybe ECKey
ecKey k = do
  guard $ 0 < k && k < maxECC_K
  return $ ECKey k

decodeECPoint :: ByteString -> Maybe (Bool, ECPoint)
decodeECPoint bs = do
  guard $ not (BS.null bs)
  (x, y) <- read
  return (isCompress, ECPoint x y)
  where
    h = BS.head bs
    body = BS.tail bs
    isCompress = h /= 4
    read | isCompress = readCompressed
         | otherwise = readUncompressed

    readUncompressed = do
      (x, y) <- splitHalf body
      guard $ y == yFromX (odd y) x
      return (x, y)

    readCompressed = do
      guard (h == 2 || h == 3)
      let isOdd = h == 3
      x <- fromBigEndianFixed body
      return (x, yFromX isOdd x)

encodeECPoint :: Bool -> ECPoint -> ByteString
encodeECPoint isCompress (ECPoint x y) = encoded
  where
    encoded | isCompress = z `BS.cons` toBigEndianFixed x
            | otherwise = 4 `BS.cons`
                          toBigEndianFixed x `BS.append` toBigEndianFixed y
    z | odd y = 3
      | otherwise = 2

normalHDNode :: Word32 -> Maybe HDNodeNormal
normalHDNode index = do
  guard $ index < 2^31
  return $ HDNodeNormal index

hardenedHDNode :: Word32 -> Maybe HDNodeHardened
hardenedHDNode index = do
  guard $ index < 2^31
  return $ HDNodeHardened index

genericHDNode :: Bool -> Word32 -> Maybe HDNode
genericHDNode b index | b = generizeHDNode <$> hardenedHDNode index
                 | otherwise = generizeHDNode <$> normalHDNode index
-- Utilities

decodeExtendData :: ByteString -> Maybe (ExtendData, ByteString)
decodeExtendData payload = do
  d: f: i: c: o: [] <- chopBS [1, 4, 4, 32] payload
  (depth, _) <- BS.uncons d
  fingerprint <- fromBigEndianFixed f
  index <- fromBigEndianFixed i
  chain <- fromBigEndianFixed c
  return (ExtendData depth fingerprint (HDNode index) chain, o)

encodeExtendData :: ExtendData -> ByteString
encodeExtendData (ExtendData d f n c) = d `BS.cons`
  toBigEndianFixed f `BS.append`
  encodeHDNode n `BS.append`
  toBigEndianFixed c

splitHalf :: BigEndianFixed a => ByteString -> Maybe (a, a)
splitHalf bs = do
  let (l, r) = BS.splitAt (BS.length bs `div` 2) bs
  vL <- fromBigEndianFixed l
  vR <- fromBigEndianFixed r
  return (vL, vR)

chopBS :: [Int64] -> ByteString -> Maybe [ByteString]
chopBS [] bs = Just [bs]
chopBS (n : ns) bs = do
  guard $ BS.length bs >= n
  let (h, t) = BS.splitAt n bs
  body <-  chopBS ns t
  return $ h: body

mkFingerprint :: ECPoint -> Word32
mkFingerprint = read4 . hash160Data . hash160 . encodeECPoint True
  where
    read4 = fromInteger . fromBigEndian . BS.take 4

toPoint :: EC.Point -> ECPoint
toPoint (EC.Point x y) = ECPoint (justWord256 x) (justWord256 y)
  where
    justWord256 = fromJust . toWord256

fromPoint :: ECPoint -> EC.Point
fromPoint (ECPoint x y) = EC.Point (toInteger x) (toInteger y)

(@*) :: Word256 -> EC.Point -> EC.Point
(@*) s p = pointMul curve (toInteger s) p

(@+) :: EC.Point -> EC.Point -> EC.Point
(@+) a b = pointAdd curve a b

yFromX :: Bool -> Word256 -> Word256
yFromX isOdd x
  | isOdd == odd y = y
  | otherwise = eccP - y
  where
    v = (7 + x `pw` 3) `mod` eccP
    y = v `pw` ((eccP + 1) `div` 4)
    pw a b
      | b == 0 = 1
      | odd b = pm $ a `pw` (b - 1)
      | otherwise = pm a `pw` (b `div` 2)
      where
        pm z = a * z `mod` eccP

-- Constants

curve@(EC.CurveFP cp) = EC.getCurveByName EC.SEC_p256k1
eccP = (fromInteger $ EC.ecc_p cp) :: Word256
eccG = EC.ecc_g $ EC.common_curve $ curve :: EC.Point

masterSeed = C8.pack "Bitcoin seed" :: ByteString
flagHardened = 1 `shift` 31
