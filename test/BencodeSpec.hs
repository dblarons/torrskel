module BencodeSpec (main, spec) where

import Test.Hspec
import Bencode

import Data.ByteString.Char8 (pack, unpack)

-- `main` is here so that this module can be run from GHCi on its own. It
-- is not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Integer encoding and decoding" $ do
    it "should encode and decode integers" $ do
      let expected = pack "i3e"
      let bin      = BInteger 3
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode negative integers" $ do
      let expected = pack "i-3e"
          bin      = BInteger (-3)
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should not pad integers with 0s" $ do
      let expected = pack "i3e"
          bin      = BInteger 0003
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
  
  describe "String encoding and decoding" $ do
    it "should encode and decode strings" $ do
      let expected = pack "4:spam"
          bin      = BString $ pack "spam"
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode the empty string" $ do
      let expected = pack "0:"
          bin      = BString $ pack ""
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
  
  describe "List encoding and decoding" $ do
    it "should encode and decode lists of one type" $ do
      let expected = pack "li3ei4ei5ee"
          bin      = BList [BInteger 3, BInteger 4, BInteger 5]
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode lists of mixed type" $ do
      let expected = pack "l4:spami3e4:eggse"
          bin      = BList [BString $ pack "spam", BInteger 3, BString $ pack "eggs"]
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode an empty list" $ do
      let expected = pack "le"
          bin      = BList []
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode a list of lists" $ do
      let expected = pack "l4:spamli3ei4eee"
          bin      = BList [BString $ pack "spam", BList [BInteger 3, BInteger 4]]
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode a list of dictionaries" $ do
      let expected = pack "ld3:cow3:mooed3:cow3:mooee"
          bin      = BList [BDict [(pack "cow", BString $ pack "moo")],
                        BDict [(pack "cow", BString $ pack "moo")]]
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
  
  describe "Dictionary encoding and decoding" $ do
    it "should encode and decode a dictionary with an integer value" $ do
      let expected = pack "d3:fooi3ee"
          bin      = BDict [(pack "foo", BInteger 3)]
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode a dictionary with a string value" $ do
      let expected = pack "d3:foo3:bare"
          bin      = BDict [(pack "foo", BString $ pack "bar")]
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode a dictionary with a list value" $ do
      let expected = pack "d3:fooli3eee"
          bin      = BDict [(pack "foo", BList [BInteger 3])]
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode a dictionary with a dictionary value" $ do
      let expected = pack "d3:food3:bari3eee"
          bin      = BDict [(pack "foo", BDict [(pack "bar", BInteger 3)])]
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin
    it "should encode and decode an empty dictionary" $ do
      let expected = pack "de"
          bin      = BDict []
      encode bin      `shouldBe` expected
      decode expected `shouldBe` Right bin

