module Fathens.Bitcoin.Binary.HashSpec (spec) where

import qualified Data.ByteString.Lazy.Char8  as C8
import           Data.Maybe                  (fromJust)
import           Fathens.Bitcoin.Binary.Hash
import           Fathens.Bitcoin.Binary.Num
import           Numeric                     (readHex, showHex)
import           Test.Hspec
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck

runTest :: IO ()
runTest = hspec spec

spec :: Spec
spec = do
  let src = "b4a9701b478d51f1e1a26a9d266bdf32c6c30295e9fd1ecfa62e1155a5"
      fromHex = toBigEndian . fst . head . readHex
      toHex i = (showHex $ fromBigEndian i) ""

  describe "hash160" $ do
    it "make hash" $ do
      let hash = toHex . hash160Data . hash160 . fromHex
      hash src `shouldBe` "85b5e78ac4446bb286e7b2eb918bbd9d45986c43"

  describe "hash256" $ do
    it "make hash" $ do
      let hash = toHex . hash256Data . hash256 . fromHex
      hash src `shouldBe` (
        "a2e9b67b831c0425e3a35cb7215d03f4c750aa2dff0c0703278c58931ce2b11b")

  describe "hmac512" $ do
    it "make hash" $ do
      let key = fromHex "1357abcdef"
          hash = toHex . hash512Data . (hmac512 key) . fromHex
      hash src `shouldBe` (
        "a2e4e539295ca4e4a5cbfdecf43aa8b81093e0a0bf6778d5ceea0392b63e384b" ++
        "1d1d8d9250ae9bc7d82a26787b9e2fdca163e7e68ee9eaad1091569390f83e13")
