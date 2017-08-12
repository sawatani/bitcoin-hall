{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Fathens.Bitcoin.Wallet.AddressSpec (spec) where

import qualified Crypto.PubKey.ECC.Types        as EC
import qualified Data.ByteString.Lazy           as BS
import           Data.Maybe                     (fromJust)
import qualified Data.Text.Lazy                 as T
import           Data.Word                      (Word64)
import           Debug.Trace
import           Fathens.Bitcoin.Binary.Base58
import           Fathens.Bitcoin.Binary.Num
import           Fathens.Bitcoin.Wallet.Address
import           Fathens.Bitcoin.Wallet.Keys
import           Numeric                        (readHex, showHex)
import           Test.Hspec
import           Test.Hspec.QuickCheck          (prop)
import           Test.QuickCheck
import           Text.Printf

runTest :: IO ()
runTest = hspec spec

spec :: Spec
spec = do
  let prvKey0 = "5JHm1YEyRk4dV8KNN4vkaFeutqpnqAVf9AWYi4cvQrtT5o57dPR"
  let prvKey1 = "5Hwgr3u458GLafKBgxtssHSPqJnYoGrSzgQsPwLFhLNYskDPyyA"
  let prvKey2 = "L1aW4aubDFB7yfras2S1mN3bqg9nwySY8nkoLmJebSLD5BWv3ENZ"
  let pubKey0 = "1EMoQ5vgsYQfZnq4RBcrRfnGEDbEry4hPY"
  let pubKey1 = "17VZNX1SN5NtKa8UQFxwQbFeFc3iqRYhem"
  let pubKey2 = "11gECtvDapMj5ZuwpvnP6Wv9MTRGxnFRs"

  describe "PrivateKey" $ do
    it "read from WIF" $ do
      let rewrite = T.unpack . base58Text .
                    prvKeyWIF .
                    fromJust . readPrivateKey . fromJust . base58 . T.pack
      rewrite prvKey0 `shouldBe` prvKey0
      rewrite prvKey1 `shouldBe` prvKey1
      rewrite prvKey2 `shouldBe` prvKey2

    it "derive PublicKey" $ do
      let derive = T.unpack . base58Text .
                   pubKeyAddress . getPublicKey .
                   fromJust . readPrivateKey . fromJust . base58 . T.pack
      derive prvKey0 `shouldBe` pubKey0
      derive prvKey1 `shouldBe` pubKey1
      derive prvKey2 `shouldBe` pubKey2

  describe "yFromX" $ do
    prop "calculate y from x" $ do
      let boomerang (EC.Point x y) = EC.Point x $ yFromX (odd y) x
      forAll anyBits256 $ \k ->
        let p = k2ec k
        in
          trace (show p) (boomerang p) `shouldBe` p

anyBits256 :: Gen Integer
anyBits256 = do
  a <- (arbitrary :: Gen Word64) `suchThat` (> 100)
  b <- (arbitrary :: Gen Word64) `suchThat` (> 100)
  c <- (arbitrary :: Gen Word64) `suchThat` (> 100)
  d <- (arbitrary :: Gen Word64) `suchThat` (> 100)
  return $ toInteger $ a * b * c * d
