module Fathens.Bitcoin.Binary.Base58Spec (main, spec) where

import           Data.Maybe                    (fromJust)
import qualified Data.Text.Lazy                as T
import           Fathens.Bitcoin.Binary.Base58
import           Fathens.Bitcoin.Binary.Num
import           Numeric                       (showHex)
import           Test.Hspec

spec :: Spec
spec = do
  describe "base58" $ do
    it "check containing chars" $ do
      (base58 $ T.pack "ablo") `shouldBe` Nothing

  describe "decodeBasee58" $ do
    it "decode hex" $
      let b58 = fromJust $ base58 $ T.pack "xpubhr25u8w"
          int = fromBigEndian $ decodeBase58 b58
      in
      (showHex int "") `shouldBe` "14dc300bd01887b6c"

  describe "decode encode, vise versa" $ do
    it "work keep leading zeros" $
      let a = T.pack "111178abxz"
          b58 = fromJust $ base58 a
          b58' = encodeBase58 $ decodeBase58 b58
      in
      (base58Text b58') `shouldBe` a

main :: IO ()
main = hspec spec
