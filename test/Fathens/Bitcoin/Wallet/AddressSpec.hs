module Fathens.Bitcoin.Wallet.AddressSpec (spec) where

import qualified Data.ByteString.Lazy           as BS
import qualified Data.ByteString.Lazy.Char8     as C8
import           Data.Maybe                     (fromJust)
import qualified Data.Text.Lazy                 as T
import           Fathens.Bitcoin.Wallet.Address
import           Numeric                        (readHex, showHex)
import           Test.Hspec
import           Test.Hspec.QuickCheck          (prop)
import           Test.QuickCheck

runTest :: IO ()
runTest = hspec spec

spec :: Spec
spec = do
  let byList = findBySymbol allPrefixes . T.pack
  let body = [1, 2, 3]

  describe "findBySymbol and appendPayload" $ do
    let prefix src = do
          p <- byList src
          return $ BS.unpack $ appendPayload p $ BS.pack body

    it "P2PKH" $ do
      prefix "14jasanlsdg" `shouldBe` Just (0x00: body)
    it "P2PKH Test" $ do
      prefix "m4jasanlsdg" `shouldBe` Just (0x6f: body)
    it "P2PKH Test" $ do
      prefix "n4jasanlsdg" `shouldBe` Just (0x6f: body)

    it "P2SH" $ do
      prefix "34jasanlsdg" `shouldBe` Just (0x05: body)
    it "P2SH Test" $ do
      prefix "24jasanlsdg" `shouldBe` Just (0xc4: body)

    it "PRV" $ do
      prefix "54jasanlsdg" `shouldBe` Just (0x80: body)
    it "PRV Test" $ do
      prefix "94jasanlsdg" `shouldBe` Just (0xef: body)

    it "CPRV" $ do
      prefix "K4jasanlsdg" `shouldBe` Just (0x80: body ++ [1])
    it "CPRV" $ do
      prefix "L4jasanlsdg" `shouldBe` Just (0x80: body ++ [1])
    it "CPRV Test" $ do
      prefix "c4jasanlsdg" `shouldBe` Just (0xef: body ++ [1])

    it "XPUB" $ do
      prefix "xpub4anlsdg" `shouldBe` Just (0x04: 0x88: 0xb2: 0x1e: body)
    it "XPUB Test" $ do
      prefix "tpubvanlsdg" `shouldBe` Just (0x04: 0x35: 0x87: 0xcf: body)

    it "XPRV" $ do
      prefix "xprv4anlsdg" `shouldBe` Just (0x04: 0x88: 0xad: 0xe4: body)
    it "XPRV Test" $ do
      prefix "tprv4anlsdg" `shouldBe` Just (0x04: 0x35: 0x83: 0x94: body)

    it "unmatch" $ do
      prefix "x4jasanlsdg" `shouldBe` Nothing

  describe "findBySymbol and getPayload" $ do
    let load src bytes = do
          p <- byList src
          b <- getPayload p $ BS.pack bytes
          return $ BS.unpack b

    it "P2PKH" $ do
      load "14jasanlsdg" (0x00: body) `shouldBe` Just body
    it "P2PKH Test" $ do
      load "m4jasanlsdg" (0x6f: body) `shouldBe` Just body
    it "P2PKH Test" $ do
      load "n4jasanlsdg" (0x6f: body) `shouldBe` Just body

    it "P2SH" $ do
      load "34jasanlsdg" (0x05: body) `shouldBe` Just body
    it "P2SH Test" $ do
      load "24jasanlsdg" (0xc4: body) `shouldBe` Just body

    it "PRV" $ do
      load "54jasanlsdg" (0x80: body) `shouldBe` Just body
    it "PRV Test" $ do
      load "94jasanlsdg" (0xef: body) `shouldBe` Just body

    it "CPRV" $ do
      load "K4jasanlsdg" (0x80: body ++ [1]) `shouldBe` Just body
    it "CPRV" $ do
      load "L4jasanlsdg" (0x80: body ++ [1]) `shouldBe` Just body
    it "CPRV Test" $ do
      load "c4jasanlsdg" (0xef: body ++ [1]) `shouldBe` Just body

    it "XPUB" $ do
      load "xpub4anlsdg" (0x04: 0x88: 0xb2: 0x1e: body) `shouldBe` Just body
    it "XPUB Test" $ do
      load "tpubvanlsdg" (0x04: 0x35: 0x87: 0xcf: body) `shouldBe` Just body

    it "XPRV" $ do
      load "xprv4anlsdg" (0x04: 0x88: 0xad: 0xe4: body) `shouldBe` Just body
    it "XPRV Test" $ do
      load "tprv4anlsdg" (0x04: 0x35: 0x83: 0x94: body) `shouldBe` Just body

    it "unmatch" $ do
      load "x4jasanlsdg" body `shouldBe` Nothing
    it "unmatch body" $ do
      load "14jasanlsdg" body `shouldBe` Nothing

allPrefixes =
  [prefixP2PKH, prefixP2SH, prefixPRV, prefixCPRV, prefixXPRV, prefixXPUB]
