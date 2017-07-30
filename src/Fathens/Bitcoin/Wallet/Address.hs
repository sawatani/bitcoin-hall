module Fathens.Bitcoin.Wallet.Address (
  AddressPrefix
, prefixP2PKH
, prefixP2SH
, prefixPRV
, prefixCPRV
, prefixXPRV
, prefixXPUB
, findBySymbol
, getPayload
, appendPayload
) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.List
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as T

-- Data

data AddressPrefix = AddressPrefix { isTest :: Bool
                   , symbols                :: [String]
                   , prefix                 :: ByteString
                   , suffix                 :: ByteString
                   } deriving (Show, Eq)

prefixP2PKH :: Bool -> AddressPrefix
prefixP2PKH False = AddressPrefix { isTest = False
                            , symbols = ["1"]
                            , prefix = BS.singleton 0x00
                            , suffix = BS.empty
                            }
prefixP2PKH True = AddressPrefix { isTest = True
                           , symbols = ["m", "n"]
                           , prefix = BS.singleton 0x6f
                           , suffix = BS.empty
                           }
prefixP2SH :: Bool -> AddressPrefix
prefixP2SH False = AddressPrefix { isTest = False
                           , symbols = ["3"]
                           , prefix = BS.singleton 0x05
                           , suffix = BS.empty
                           }
prefixP2SH True = AddressPrefix { isTest = True
                          , symbols = ["2"]
                          , prefix = BS.singleton 0xc4
                          , suffix = BS.empty
                          }
prefixPRV :: Bool -> AddressPrefix
prefixPRV False = AddressPrefix { isTest = False
                          , symbols = ["5"]
                          , prefix = BS.singleton 0x80
                          , suffix = BS.empty
                          }
prefixPRV True = AddressPrefix { isTest = True
                         , symbols = ["9"]
                         , prefix = BS.singleton 0xef
                         , suffix = BS.empty
                         }
prefixCPRV :: Bool -> AddressPrefix
prefixCPRV False = AddressPrefix { isTest = False
                           , symbols = ["K", "L"]
                           , prefix = BS.singleton 0x80
                           , suffix = BS.singleton 0x01
                           }
prefixCPRV True = AddressPrefix { isTest = True
                          , symbols = ["c"]
                          , prefix = BS.singleton 0xef
                          , suffix = BS.singleton 0x01
                          }
prefixXPUB :: Bool -> AddressPrefix
prefixXPUB False = AddressPrefix { isTest = False
                           , symbols = ["xpub"]
                           , prefix = BS.pack [0x04, 0x88, 0xb2, 0x1e]
                           , suffix = BS.empty
                           }
prefixXPUB True = AddressPrefix { isTest = True
                          , symbols = ["tpub"]
                          , prefix = BS.pack [0x04, 0x35, 0x87, 0xcf]
                          , suffix = BS.empty
                          }
prefixXPRV :: Bool -> AddressPrefix
prefixXPRV False = AddressPrefix { isTest = False
                           , symbols = ["xprv"]
                           , prefix = BS.pack [0x04, 0x88, 0xad, 0xe4]
                           , suffix = BS.empty
                           }
prefixXPRV True = AddressPrefix { isTest = True
                          , symbols = ["tprv"]
                          , prefix = BS.pack [0x04, 0x35, 0x83, 0x94]
                          , suffix = BS.empty
                          }

-- Functions

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

findBySymbol :: [Bool -> AddressPrefix] -> Text -> Maybe AddressPrefix
findBySymbol as t = find (flip isMatchSymbol t) pres
  where
    pres = as <*> [True, False]

getPayload :: AddressPrefix -> ByteString -> ByteString
getPayload p src = BS.take net $ BS.drop lenP src
  where
    lenP = BS.length $ prefix p
    lenS = BS.length $ suffix p
    net = BS.length src - lenP - lenS

appendPayload :: AddressPrefix -> ByteString -> ByteString
appendPayload p src = BS.concat [prefix p, src, suffix p]
