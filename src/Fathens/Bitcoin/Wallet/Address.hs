module Fathens.Bitcoin.Wallet.Address (
  Prefix_(isTestnet)
, Prefix(..)
, isCompressing
, findBySymbol
, getPayload
, appendPayload
) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Lazy          (ByteString)
import qualified Data.ByteString.Lazy          as BS
import           Data.List
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as T
import           Fathens.Bitcoin.Binary.Base58

-- Data

data Prefix
  = PrefixP2PKH Bool
  | PrefixP2SH Bool
  | PrefixPRV Bool
  | PrefixCPRV Bool
  | PrefixXPUB Bool
  | PrefixXPRV Bool
  deriving (Show, Eq)

instance Prefix_ Prefix where
  isTestnet (PrefixP2PKH b) = b
  isTestnet (PrefixP2SH b)  = b
  isTestnet (PrefixPRV b)   = b
  isTestnet (PrefixCPRV b)  = b
  isTestnet (PrefixXPUB b)  = b
  isTestnet (PrefixXPRV b)  = b

  symbols p@(PrefixP2PKH _) | isTestnet p = ["m", "n"]
                            | otherwise = ["1"]
  symbols p@(PrefixP2SH _) | isTestnet p = ["2"]
                           | otherwise = ["3"]
  symbols p@(PrefixPRV _) | isTestnet p = ["9"]
                          | otherwise = ["5"]
  symbols p@(PrefixCPRV _) | isTestnet p = ["c"]
                           | otherwise = ["K", "L"]
  symbols p@(PrefixXPUB _) | isTestnet p = ["tpub"]
                           | otherwise = ["xpub"]
  symbols p@(PrefixXPRV _) | isTestnet p = ["tprv"]
                           | otherwise = ["xprv"]

  prefix p@(PrefixP2PKH _) | isTestnet p = BS.singleton 0x6f
                           | otherwise = BS.singleton 0x00
  prefix p@(PrefixP2SH _) | isTestnet p = BS.singleton 0xc4
                          | otherwise = BS.singleton 0x05
  prefix p@(PrefixPRV _) | isTestnet p = BS.singleton 0xef
                         | otherwise = BS.singleton 0x80
  prefix p@(PrefixCPRV _) | isTestnet p = BS.singleton 0xef
                          | otherwise = BS.singleton 0x80
  prefix p@(PrefixXPUB _) | isTestnet p = BS.pack [0x04, 0x35, 0x87, 0xcf]
                          | otherwise = BS.pack [0x04, 0x88, 0xb2, 0x1e]
  prefix p@(PrefixXPRV _) | isTestnet p = BS.pack [0x04, 0x35, 0x83, 0x94]
                          | otherwise = BS.pack [0x04, 0x88, 0xad, 0xe4]

  suffix (PrefixCPRV b) = BS.singleton 0x01
  suffix _              = BS.empty

-- Classes

class Prefix_ a where
  isTestnet :: a -> Bool
  symbols :: a -> [String]
  prefix :: a -> ByteString
  suffix :: a -> ByteString

-- Functions

isCompressing :: Prefix -> Maybe Bool
isCompressing (PrefixPRV _)  = Just False
isCompressing (PrefixCPRV _) = Just True
isCompressing _              = Nothing

findBySymbol :: [Bool -> Prefix] -> Base58 -> Maybe Prefix
findBySymbol s b = find (flip isMatchSymbol t) pres
  where
    t = base58Text b
    pres = s <*> [True, False]

getPayload :: Prefix_ a => a -> ByteString -> Maybe ByteString
getPayload p src = do
  guard $ isMatchPayload p src
  return $ BS.take net $ BS.drop lenP src
  where
    lenP = BS.length $ prefix p
    lenS = BS.length $ suffix p
    net = BS.length src - lenP - lenS

appendPayload :: Prefix_ a => a -> ByteString -> ByteString
appendPayload p src = BS.concat [prefix p, src, suffix p]

-- Utilities

isMatchSymbol :: Prefix_ a => a -> Text -> Bool
isMatchSymbol p t = any isPrefix pres
  where
    isPrefix = flip T.isPrefixOf t
    pres = map T.pack $ symbols p

isMatchPayload :: Prefix_ a => a -> ByteString -> Bool
isMatchPayload p s = isPre && isSuf
  where
    isPre = BS.isPrefixOf (prefix p) s
    isSuf = BS.isSuffixOf (suffix p) s
