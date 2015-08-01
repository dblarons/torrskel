module MetaInfoFileSpec (main, spec) where

import Test.Hspec
import MetaInfoFile
import Bencode

import Data.ByteString.Char8 (pack, unpack)

-- `main` is here so that this module can be run from GHCi on its own. It
-- is not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "NOT YET IMPLEMENTED" $ do
    it "NYI" $ do
      1 `shouldBe` 2


