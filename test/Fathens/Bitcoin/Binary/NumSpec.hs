module Fathens.Bitcoin.Binary.NumSpec (spec) where

import qualified Data.ByteString.Lazy       as BS
import           Data.Word                  (Word64)
import           Fathens.Bitcoin.Binary.Num
import           Numeric                    (readHex, showHex)
import           Test.Hspec
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck

runTest :: IO ()
runTest = hspec spec

spec :: Spec
spec = do
  describe "toBigEndian" $ do
    it "make bytes" $ do
      let bytes = BS.unpack . toBigEndian
      bytes 0x1234567890abcdef `shouldBe` [
        0x12, 0x34, 0x56, 0x78, 0x90, 0xab, 0xcd, 0xef]

  describe "putBigEndianFixed" $ do
    it "make bytes" $ do
      let bytes = BS.unpack . putBigEndianFixed 10
      bytes 0x1234567890abcdef `shouldBe` [
        0, 0, 0x12, 0x34, 0x56, 0x78, 0x90, 0xab, 0xcd, 0xef]

  describe "fromBigEndian" $ do
    prop "work vise versa with toBigEndian" $ do
      let boomerang = fromInteger . fromBigEndian . toBigEndian . toInteger
      forAll (arbitrary :: Gen Word64) $ \i ->
        boomerang i `shouldBe` i
    prop "work vise versa with toBigEndianFixed" $ do
      let boomerang n = fromInteger . fromBigEndian .
                        putBigEndianFixed n . toInteger
      forAll (choose (8, 20) :: Gen Word64) $ \n ->
        forAll (arbitrary :: Gen Word64) $ \i ->
        boomerang n i `shouldBe` i
