module Fathens.Bitcoin.Wallet.KeysSpec (spec) where

import qualified Crypto.PubKey.ECC.Types        as EC
import qualified Data.ByteString.Lazy           as BS
import           Data.Maybe
import qualified Data.Text.Lazy                 as T
import           Data.Word                      (Word32, Word64)
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
                    toBase58 . fromJust .
                    (fromBase58 :: Base58 -> Maybe PrvKey) .
                    fromJust . base58 . T.pack
      rewrite prvKey0 `shouldBe` prvKey0
      rewrite prvKey1 `shouldBe` prvKey1
      rewrite prvKey2 `shouldBe` prvKey2

    it "derive PublicKey" $ do
      let derive = T.unpack . base58Text .
                   toBase58 . toPublicKey . fromJust .
                   (fromBase58 :: Base58 -> Maybe PrvKey) .
                   fromJust . base58 . T.pack
      derive prvKey0 `shouldBe` pubKey0
      derive prvKey1 `shouldBe` pubKey1
      derive prvKey2 `shouldBe` pubKey2

  describe "ECPoint" $ do
    let getPoint = toPublicKey . fromJust . ecKey
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
          boomerang True p `shouldBe` (True, p)

  describe "Deriving HD Keys" $ do
    it "Test vector 1" $ do
      let (m: h0: n1: h2: n2: n10: []) =
            mkChain "000102030405060708090a0b0c0d0e0f"
            [(True, 0), (False, 1), (True, 2), (False, 2), (False, 1000000000)]
      (fst m) `shouldBe` "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8"
      (snd m) `shouldBe` "xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi"
      (fst h0) `shouldBe` "xpub68Gmy5EdvgibQVfPdqkBBCHxA5htiqg55crXYuXoQRKfDBFA1WEjWgP6LHhwBZeNK1VTsfTFUHCdrfp1bgwQ9xv5ski8PX9rL2dZXvgGDnw"
      (snd h0) `shouldBe` "xprv9uHRZZhk6KAJC1avXpDAp4MDc3sQKNxDiPvvkX8Br5ngLNv1TxvUxt4cV1rGL5hj6KCesnDYUhd7oWgT11eZG7XnxHrnYeSvkzY7d2bhkJ7"
      (fst n1) `shouldBe` "xpub6ASuArnXKPbfEwhqN6e3mwBcDTgzisQN1wXN9BJcM47sSikHjJf3UFHKkNAWbWMiGj7Wf5uMash7SyYq527Hqck2AxYysAA7xmALppuCkwQ"
      (snd n1) `shouldBe` "xprv9wTYmMFdV23N2TdNG573QoEsfRrWKQgWeibmLntzniatZvR9BmLnvSxqu53Kw1UmYPxLgboyZQaXwTCg8MSY3H2EU4pWcQDnRnrVA1xe8fs"
      (fst h2) `shouldBe` "xpub6D4BDPcP2GT577Vvch3R8wDkScZWzQzMMUm3PWbmWvVJrZwQY4VUNgqFJPMM3No2dFDFGTsxxpG5uJh7n7epu4trkrX7x7DogT5Uv6fcLW5"
      (snd h2) `shouldBe` "xprv9z4pot5VBttmtdRTWfWQmoH1taj2axGVzFqSb8C9xaxKymcFzXBDptWmT7FwuEzG3ryjH4ktypQSAewRiNMjANTtpgP4mLTj34bhnZX7UiM"
      (fst n2) `shouldBe` "xpub6FHa3pjLCk84BayeJxFW2SP4XRrFd1JYnxeLeU8EqN3vDfZmbqBqaGJAyiLjTAwm6ZLRQUMv1ZACTj37sR62cfN7fe5JnJ7dh8zL4fiyLHV"
      (snd n2) `shouldBe` "xprvA2JDeKCSNNZky6uBCviVfJSKyQ1mDYahRjijr5idH2WwLsEd4Hsb2Tyh8RfQMuPh7f7RtyzTtdrbdqqsunu5Mm3wDvUAKRHSC34sJ7in334"
      (fst n10) `shouldBe` "xpub6H1LXWLaKsWFhvm6RVpEL9P4KfRZSW7abD2ttkWP3SSQvnyA8FSVqNTEcYFgJS2UaFcxupHiYkro49S8yGasTvXEYBVPamhGW6cFJodrTHy"
      (snd n10) `shouldBe` "xprvA41z7zogVVwxVSgdKUHDy1SKmdb533PjDz7J6N6mV6uS3ze1ai8FHa8kmHScGpWmj4WggLyQjgPie1rFSruoUihUZREPSL39UNdE3BBDu76"

    it "Test vector 2" $ do
      let (m: n0: h7: n1: h6: n2: []) =
            mkChain "fffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542"
            [(False, 0), (True, 2147483647), (False, 1), (True, 2147483646), (False, 2)]
      (fst m) `shouldBe` "xpub661MyMwAqRbcFW31YEwpkMuc5THy2PSt5bDMsktWQcFF8syAmRUapSCGu8ED9W6oDMSgv6Zz8idoc4a6mr8BDzTJY47LJhkJ8UB7WEGuduB"
      (snd m) `shouldBe` "xprv9s21ZrQH143K31xYSDQpPDxsXRTUcvj2iNHm5NUtrGiGG5e2DtALGdso3pGz6ssrdK4PFmM8NSpSBHNqPqm55Qn3LqFtT2emdEXVYsCzC2U"
      (fst n0) `shouldBe` "xpub69H7F5d8KSRgmmdJg2KhpAK8SR3DjMwAdkxj3ZuxV27CprR9LgpeyGmXUbC6wb7ERfvrnKZjXoUmmDznezpbZb7ap6r1D3tgFxHmwMkQTPH"
      (snd n0) `shouldBe` "xprv9vHkqa6EV4sPZHYqZznhT2NPtPCjKuDKGY38FBWLvgaDx45zo9WQRUT3dKYnjwih2yJD9mkrocEZXo1ex8G81dwSM1fwqWpWkeS3v86pgKt"
      (fst h7) `shouldBe` "xpub6ASAVgeehLbnwdqV6UKMHVzgqAG8Gr6riv3Fxxpj8ksbH9ebxaEyBLZ85ySDhKiLDBrQSARLq1uNRts8RuJiHjaDMBU4Zn9h8LZNnBC5y4a"
      (snd h7) `shouldBe` "xprv9wSp6B7kry3Vj9m1zSnLvN3xH8RdsPP1Mh7fAaR7aRLcQMKTR2vidYEeEg2mUCTAwCd6vnxVrcjfy2kRgVsFawNzmjuHc2YmYRmagcEPdU9"
      (fst n1) `shouldBe` "xpub6DF8uhdarytz3FWdA8TvFSvvAh8dP3283MY7p2V4SeE2wyWmG5mg5EwVvmdMVCQcoNJxGoWaU9DCWh89LojfZ537wTfunKau47EL2dhHKon"
      (snd n1) `shouldBe` "xprv9zFnWC6h2cLgpmSA46vutJzBcfJ8yaJGg8cX1e5StJh45BBciYTRXSd25UEPVuesF9yog62tGAQtHjXajPPdbRCHuWS6T8XA2ECKADdw4Ef"
      (fst h6) `shouldBe` "xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL"
      (snd h6) `shouldBe` "xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc"
      (fst n2) `shouldBe` "xpub6FnCn6nSzZAw5Tw7cgR9bi15UV96gLZhjDstkXXxvCLsUXBGXPdSnLFbdpq8p9HmGsApME5hQTZ3emM2rnY5agb9rXpVGyy3bdW6EEgAtqt"
      (snd n2) `shouldBe` "xprvA2nrNbFZABcdryreWet9Ea4LvTJcGsqrMzxHx98MMrotbir7yrKCEXw7nadnHM8Dq38EGfSh6dqA9QWTyefMLEcBYJUuekgW4BYPJcr9E7j"

    it "Test vector 3" $ do
      let (m: h0: []) =
            mkChain "4b381541583be4423346c643850da4b320e46a87ae3d2a4e6da11eba819cd4acba45d239319ac14f863b8d5ab5a0d0c64d2e8a1e7d1457df2e5a3c51c73235be"
            [(True, 0)]
      (fst m) `shouldBe` "xpub661MyMwAqRbcEZVB4dScxMAdx6d4nFc9nvyvH3v4gJL378CSRZiYmhRoP7mBy6gSPSCYk6SzXPTf3ND1cZAceL7SfJ1Z3GC8vBgp2epUt13"
      (snd m) `shouldBe` "xprv9s21ZrQH143K25QhxbucbDDuQ4naNntJRi4KUfWT7xo4EKsHt2QJDu7KXp1A3u7Bi1j8ph3EGsZ9Xvz9dGuVrtHHs7pXeTzjuxBrCmmhgC6"
      (fst h0) `shouldBe` "xpub68NZiKmJWnxxS6aaHmn81bvJeTESw724CRDs6HbuccFQN9Ku14VQrADWgqbhhTHBaohPX4CjNLf9fq9MYo6oDaPPLPxSb7gwQN3ih19Zm4Y"
      (snd h0) `shouldBe` "xprv9uPDJpEQgRQfDcW7BkF7eTya6RPxXeJCqCJGHuCJ4GiRVLzkTXBAJMu2qaMWPrS7AANYqdq6vcBcBUdJCVVFceUvJFjaPdGZ2y9WACViL4L"

    prop "derivePublicKey" $ do
      forAll arbitrary $ \root ->
        forAll genNodePath $ \path ->
        forAll arbitrary $ \node ->
        let parent = last $ mkChildren path root :: HDPrvKey
            prvSide = toExtendPublicKey . flip derivePrivateKey node
            pubSize = flip derivePublicKey node . toExtendPublicKey
        in
          prvSide (trace (show parent) parent) `shouldBe` pubSize parent

anyBits256 :: Gen Word256
anyBits256 = choose (minBound, maxECC_K)

mkChain :: String -> [(Bool, Word32)] -> [(String, String)]
mkChain seed ps = (asPair master): map asPair children
  where
    master = toXPrvKey False $ fromJust $
      exKeyFromSeed $ fst $ head $ readHex seed
    ns = map (\(b, i) -> fromJust $ mkHDNode b i) ps
    children = mkChildren ns master

mkChildren :: ExtendPrivateKey a => [HDNode'] -> a -> [a]
mkChildren [] prv = []
mkChildren (node: ns) prv = prv': mkChildren ns prv'
  where
    prv' = derivePrivateKey prv node

asPair :: XPrvKey -> (String, String)
asPair prv = (toString $ toBase58 $ toExtendPublicKey prv,
              toString $ toBase58 prv)
  where
    toString = T.unpack . base58Text

genNodePath :: Gen [HDNode']
genNodePath = do
  n <- choose (2, 5)
  v <- vector n
  return v

instance Arbitrary HDNodeNormal where
  arbitrary = do
    index <- choose (1, 2^31 - 1)
    return $ fromJust $ normalHDNode index

instance Arbitrary HDNode' where
  arbitrary = do
    isH <- arbitrary
    index <- choose (1, 2^31 - 1)
    return $ fromJust $ mkHDNode isH index

instance Arbitrary HDPrvKey where
  arbitrary = do
    let seed = choose (toInteger 2^10, 2^500)
    mkey <- suchThat (exKeyFromSeed <$> seed) isJust
    return $ fromJust mkey
