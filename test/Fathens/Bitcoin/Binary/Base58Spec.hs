module Fathens.Bitcoin.Binary.Base58Spec (spec) where

import qualified Data.ByteString.Lazy.Char8    as C8
import           Data.Maybe                    (fromJust, isJust)
import qualified Data.Text.Lazy                as T
import           Fathens.Bitcoin.Binary.Base58
import           Fathens.Bitcoin.Binary.Num
import           Numeric                       (readHex, showHex)
import           Test.Hspec
import           Test.Hspec.QuickCheck         (prop)
import           Test.QuickCheck

runTest :: IO ()
runTest = hspec spec

spec :: Spec
spec = do
  describe "base58" $ do
    it "check containing chars" $ do
      (base58 $ T.pack "ablo") `shouldBe` Nothing
    it "check not empty" $ do
      (base58 $ T.pack "") `shouldBe` Nothing
    it "should be fine" $ do
      (isJust $ base58 $ T.pack
       "123456789ABCDEFGHJKLMNPQRSTUVWXYzabcdefghijkmnopqrstuvwxyz")
        `shouldBe` True

  describe "decodeBasee58" $ do
    it "decode hex" $
      let b58 = fromJust $ base58 $
            T.pack "2AF1EDCBxyzwvutsrqponmkjihgfedcbaZYXWVUTSRQPNMLKJHG9876543"
          int = fromBigEndian $ decodeBase58 b58
      in
      (showHex int "") `shouldBe`
      "45770affd9d5708e474b69220c9dc7f67faed1273a" ++
      "3ccdd044d489563a9d312869dab70c3521bd88d748"

  describe "decode encode, vise versa" $ do
    prop "work keep leading zeros" $
      forAll (choose (1, 10)) $ \n ->
      forAll base58chars $ \s ->
      prop_Base58String $ (replicate n '1') ++ s
    prop "work on randam" $
      forAll base58chars $ prop_Base58String

  describe "base58check" $ do
    it "decode" $ do
      let checked = T.pack "1AfMq1PQAhteQ5wffCdX3Yb8VcnJawHL6N"
          hex = "69fa531ef7022dbdd51b6cfdcfa14493d85f7807"
          dst = fromJust $ decodeBase58Check $ fromJust $ base58 $ checked
          int = fromBigEndian dst
      (showHex int "" )`shouldBe` hex
    it "work vise versa" $ do
      let checked = T.pack "1AfMq1PQAhteQ5wffCdX3Yb8VcnJawHL6N"
          dst = fromJust $ decodeBase58Check $ fromJust $ base58 $ checked
          bc = encodeBase58Check dst
      (base58Text bc)`shouldBe` checked

prop_Base58String :: String -> Bool
prop_Base58String s = (base58Text b58') == a
  where
    a = T.pack s
    b58 = fromJust $ base58 a
    b58' = encodeBase58 $ decodeBase58 b58

base58chars :: Gen String
base58chars = listOf1 $
  elements "123456789ABCDEFGHJKLMNPQRSTUVWXYzabcdefghijkmnopqrstuvwxyz"
