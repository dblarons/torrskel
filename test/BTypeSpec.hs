module BTypeSpec
  ( main
  , spec
  ) where

import Data.ByteString.Char8 (pack)
import Test.Hspec

import BType

-- `main` is here so that this module can be run from GHCi on its own. It
-- is not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "BType" $ do
    it "should unwrap BDict" $ do
      let expected = Just [(pack "foo", BInteger 99)]
          encoded = BDict [(pack "foo", BInteger 99)]
      fromBType encoded `shouldBe` expected
    it "should unwrap BString" $ do
      let expected = Just $ pack "foo"
          encoded = BString $ pack "foo"
      fromBType encoded `shouldBe` expected
    it "should unwrap BInteger" $ do
      let expected = Just 99
          encoded = BInteger 99
      fromBType encoded `shouldBe` expected
    it "should unwrap BLists composed of only BStrings" $ do
      let expected = Just [pack "foo", pack "bar", pack "baz"]
          encoded =
            BList
              [BString $ pack "foo", BString $ pack "bar", BString $ pack "baz"]
      fromBType encoded `shouldBe` expected
