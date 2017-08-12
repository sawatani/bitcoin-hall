module Fathens.Bitcoin.Wallet.KeysSpec (spec) where

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

  describe "ECPoint" $ do
    let getPoint = getPublicECPoint . fromJust . ecKey
    let boomerang isCompress =
          fromJust . decodeECPoint . encodeECPoint isCompress
    prop "encode length (uncompressed)" $ do
      forAll anyBits256 $ \k ->
        let bs = encodeECPoint False $ getPoint k
        in
          BS.length bs `shouldBe` (256 `div` 8 * 2 + 1)
    prop "encode length (compressed)" $ do
      forAll anyBits256 $ \k ->
        let bs = encodeECPoint True $ getPoint k
        in
          BS.length bs `shouldBe` (256 `div` 8 + 1)
    prop "decode/encode, vise versa (uncompressed)" $ do
      forAll anyBits256 $ \k ->
        let p =  getPoint k
        in
          boomerang False p `shouldBe` (False, p)
    prop "decode/encode, vise versa (compressed)" $ do
      forAll anyBits256 $ \k ->
        let p =  getPoint k
        in
          trace (show p) (boomerang True p) `shouldBe` (True, p)

anyBits256 :: Gen Word256
anyBits256 = choose (minBound, maxECC_K)
