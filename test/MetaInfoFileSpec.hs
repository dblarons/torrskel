module MetaInfoFileSpec (main, spec) where

import Test.Hspec
import Data.ByteString.Char8 (pack)

import MetaInfoFile
import BType

{-# ANN module "HLint: ignore Redundant do" #-}

-- `main` is here so that this module can be run from GHCi on its own. It
-- is not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "MetaInfoFile" $ do
      let metaInfo = MetaInfo { metaInfoName = pack "foo"
                              , metaInfoPieceLength = 0
                              , metaInfoPieces = pack "foo"
                              , metaInfoLength = Just 10
                              , metaInfoFiles = Nothing
                              }
          metaInfoEncoded = BDict [(pack "name", BString $ pack "foo"),
                                  (pack "piece length", BInteger 0),
                                  (pack "pieces", BString $ pack "foo"),
                                  (pack "length", BInteger 10)]

      describe "extractTorrentMeta" $ do
        it "should extract a torrent file BDict into a record" $ do
          let expected = TorrentMeta { torrentMetaAnnounce = pack "foobar"
                                     , torrentMetaInfo = metaInfo
                                     }
              encoded = BDict [(pack "announce", BString $ pack "foobar"),
                              (pack "info", metaInfoEncoded)]
          extractTorrentMeta encoded `shouldBe` Just expected

      describe "extractMetaInfo" $ do
        it "should extract a meta info BDict into a record" $ do
          extractMetaInfo metaInfoEncoded `shouldBe` Just metaInfo

      describe "extractMetaFile" $ do
        it "should extract a meta file BDict into a record" $ do
          let expected = MetaFile { metaFileLength = 100
                                  , metaFilePath = [pack "foo", pack "bar"]
                                  }
              encoded = BDict [(pack "length", BInteger 100),
                              (pack "path", BList [BString $ pack "foo",
                                                  BString $ pack "bar"])]
          extractMetaFile encoded `shouldBe` Just expected


