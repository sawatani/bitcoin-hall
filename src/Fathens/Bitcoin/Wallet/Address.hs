module Fathens.Bitcoin.Wallet.Address (
  AddressPrefix(isTestnet)
, prefixP2PKH
, prefixP2SH
, prefixPRV
, prefixCPRV
, prefixXPRV
, prefixXPUB
, findBySymbol
, getPayload
, appendPayload
, isCompressed
) where

import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.List
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as T

-- Data

data AddressPrefix = AddressPrefix {
  isTestnet :: Bool
, symbols   :: [String]
, prefix    :: ByteString
, suffix    :: ByteString
} deriving (Show, Eq)

prefixP2PKH :: Bool -> AddressPrefix
prefixP2PKH False = singleton_P2PKH_False
prefixP2PKH True  = singleton_P2PKH_True
singleton_P2PKH_False = AddressPrefix {
  isTestnet = False
, symbols = ["1"]
, prefix = BS.singleton 0x00
, suffix = BS.empty
}
singleton_P2PKH_True = AddressPrefix {
  isTestnet = True
, symbols = ["m", "n"]
, prefix = BS.singleton 0x6f
, suffix = BS.empty
}
prefixP2SH :: Bool -> AddressPrefix
prefixP2SH False = singleton_P2SH_False
prefixP2SH True  = singleton_P2SH_True
singleton_P2SH_False = AddressPrefix {
  isTestnet = False
, symbols = ["3"]
, prefix = BS.singleton 0x05
, suffix = BS.empty
}
singleton_P2SH_True = AddressPrefix {
  isTestnet = True
, symbols = ["2"]
, prefix = BS.singleton 0xc4
, suffix = BS.empty
}
prefixPRV :: Bool -> AddressPrefix
prefixPRV False = singleton_PRV_False
prefixPRV True  = singleton_PRV_True
singleton_PRV_False = AddressPrefix {
  isTestnet = False
, symbols = ["5"]
, prefix = BS.singleton 0x80
, suffix = BS.empty
}
singleton_PRV_True = AddressPrefix {
  isTestnet = True
, symbols = ["9"]
, prefix = BS.singleton 0xef
, suffix = BS.empty
}
prefixCPRV :: Bool -> AddressPrefix
prefixCPRV False = singleton_CPRV_False
prefixCPRV True  = singleton_CPRV_True
singleton_CPRV_False = AddressPrefix {
  isTestnet = False
, symbols = ["K", "L"]
, prefix = BS.singleton 0x80
, suffix = BS.singleton 0x01
}
singleton_CPRV_True = AddressPrefix {
  isTestnet = True
, symbols = ["c"]
, prefix = BS.singleton 0xef
, suffix = BS.singleton 0x01
}
prefixXPUB :: Bool -> AddressPrefix
prefixXPUB False = singleton_XPUB_False
prefixXPUB True  = singleton_XPUB_True
singleton_XPUB_False = AddressPrefix {
  isTestnet = False
, symbols = ["xpub"]
, prefix = BS.pack [0x04, 0x88, 0xb2, 0x1e]
, suffix = BS.empty
}
singleton_XPUB_True = AddressPrefix {
  isTestnet = True
, symbols = ["tpub"]
, prefix = BS.pack [0x04, 0x35, 0x87, 0xcf]
, suffix = BS.empty
}
prefixXPRV :: Bool -> AddressPrefix
prefixXPRV False = singleton_XPRV_False
prefixXPRV True  = singleton_XPRV_True
singleton_XPRV_False = AddressPrefix {
  isTestnet = False
, symbols = ["xprv"]
, prefix = BS.pack [0x04, 0x88, 0xad, 0xe4]
, suffix = BS.empty
}
singleton_XPRV_True = AddressPrefix {
  isTestnet = True
, symbols = ["tprv"]
, prefix = BS.pack [0x04, 0x35, 0x83, 0x94]
, suffix = BS.empty
}

-- Classes

-- Functions

findBySymbol :: [Bool -> AddressPrefix] -> Text -> Maybe AddressPrefix
findBySymbol as t = find (flip isMatchSymbol t) pres
  where
    pres = as <*> [True, False]

getPayload :: AddressPrefix -> ByteString -> Maybe ByteString
getPayload p src = do
  guard $ isMatchPayload p src
  return $ BS.take net $ BS.drop lenP src
  where
    lenP = BS.length $ prefix p
    lenS = BS.length $ suffix p
    net = BS.length src - lenP - lenS

appendPayload :: AddressPrefix -> ByteString -> ByteString
appendPayload p src = BS.concat [prefix p, src, suffix p]

isCompressed :: AddressPrefix -> Bool -- TODO classify
isCompressed a | a == singleton_CPRV_False = True
               | a == singleton_CPRV_True  = True
               | a == singleton_PRV_False  = False
               | a == singleton_PRV_True   = False
               | otherwise                = undefined

-- Utilities

isMatchSymbol :: AddressPrefix -> Text -> Bool
isMatchSymbol p t = any isPrefix pres
  where
    isPrefix = flip T.isPrefixOf t
    pres = map T.pack $ symbols p

isMatchPayload :: AddressPrefix -> ByteString -> Bool
isMatchPayload p s = isPre && isSuf
  where
    isPre = BS.isPrefixOf (prefix p) s
    isSuf = BS.isSuffixOf (suffix p) s
