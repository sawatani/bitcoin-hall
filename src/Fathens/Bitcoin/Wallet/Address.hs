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

  symbols (PrefixP2PKH b) | b == True = ["m", "n"]
                          | b == False = ["1"]
  symbols (PrefixP2SH b) | b == True = ["2"]
                         | b == False = ["3"]
  symbols (PrefixPRV b) | b == True = ["9"]
                        | b == False = ["5"]
  symbols (PrefixCPRV b) | b == True = ["c"]
                         | b == False = ["K", "L"]
  symbols (PrefixXPUB b) | b == True = ["tpub"]
                         | b == False = ["xpub"]
  symbols (PrefixXPRV b) | b == True = ["tprv"]
                         | b == False = ["xprv"]

  prefix (PrefixP2PKH b) | b == True = BS.singleton 0x6f
                        | b == False = BS.singleton 0x00
  prefix (PrefixP2SH b) | b == True = BS.singleton 0xc4
                        | b == False = BS.singleton 0x05
  prefix (PrefixPRV b) | b == True = BS.singleton 0xef
                       | b == False = BS.singleton 0x80
  prefix (PrefixCPRV b) | b == True = BS.singleton 0xef
                        | b == False = BS.singleton 0x80
  prefix (PrefixXPUB b) | b == True = BS.pack [0x04, 0x35, 0x87, 0xcf]
                        | b == False = BS.pack [0x04, 0x88, 0xb2, 0x1e]
  prefix (PrefixXPRV b) | b == True = BS.pack [0x04, 0x35, 0x83, 0x94]
                        | b == False = BS.pack [0x04, 0x88, 0xad, 0xe4]

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

findBySymbol :: Base58 -> Maybe Prefix
findBySymbol b = find (flip isMatchSymbol t) pres
  where
    t = base58Text b
    s = [
        PrefixP2PKH
      , PrefixP2SH
      , PrefixPRV
      , PrefixCPRV
      , PrefixXPRV
      , PrefixXPUB
      ]
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
