module Fathens.Bitcoin.Wallet.Address where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.List
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as T

-- Data

data P2PKH = P2PKH Bool deriving (Show, Eq)
data P2SH = P2SH Bool deriving (Show, Eq)
data PRV = PRV Bool deriving (Show, Eq)
data CPRV = CPRV Bool deriving (Show, Eq)
data XPUB = XPUB Bool deriving (Show, Eq)
data XPRV = XPRV Bool deriving (Show, Eq)

-- Classes

class AddressPrefix a where
  isTest :: a -> Bool
  symbols :: a -> [String]
  prefix :: a -> ByteString

  suffix :: a -> ByteString
  suffix _ = BS.empty

  isMatchSymbol :: a -> Text -> Bool
  isMatchSymbol p t = any isPrefix pres
    where
      isPrefix = flip T.isPrefixOf t
      pres = map T.pack $ symbols p

  isMatchPayload :: a -> ByteString -> Bool
  isMatchPayload p s = isPre && isSuf
    where
      isPre = BS.isPrefixOf (prefix p) s
      isSuf = BS.isSuffixOf (suffix p) s

instance AddressPrefix P2PKH where
  isTest (P2PKH b) = b
  symbols (P2PKH False) = ["1"]
  symbols (P2PKH True)  = ["m", "n"]
  prefix (P2PKH False) = BS.singleton 0x00
  prefix (P2PKH True)  = BS.singleton 0x6f

instance AddressPrefix P2SH where
  isTest (P2SH b) = b
  symbols (P2SH False) = ["3"]
  symbols (P2SH True)  = ["2"]
  prefix (P2SH False) = BS.singleton 0x05
  prefix (P2SH True)  = BS.singleton 0xc4

instance AddressPrefix XPUB where
  isTest (XPUB b) = b
  symbols (XPUB False) = ["xpub"]
  symbols (XPUB True)  = ["tpub"]
  prefix (XPUB False) = BS.pack [0x04, 0x88, 0xb2, 0x1e]
  prefix (XPUB True)  = BS.pack [0x04, 0x35, 0x87, 0xcf]

instance AddressPrefix XPRV where
  isTest (XPRV b) = b
  symbols (XPRV False) = ["xprv"]
  symbols (XPRV True)  = ["tprv"]
  prefix (XPRV False) = BS.pack [0x04, 0x88, 0xad, 0xe4]
  prefix (XPRV True)  = BS.pack [0x04, 0x35, 0x83, 0x94]

instance AddressPrefix PRV where
  isTest (PRV b) = b
  symbols (PRV False) = ["5"]
  symbols (PRV True)  = ["9"]
  prefix (PRV False) = BS.singleton 0x80
  prefix (PRV True)  = BS.singleton 0xef

instance AddressPrefix CPRV where
  isTest (CPRV b) = b
  symbols (CPRV False) = ["K", "L"]
  symbols (CPRV True)  = ["c"]
  prefix (CPRV False) = BS.singleton 0x80
  prefix (CPRV True)  = BS.singleton 0xef
  suffix _ = BS.singleton 0x01
