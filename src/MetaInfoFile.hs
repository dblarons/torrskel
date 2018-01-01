{-# LANGUAGE OverloadedStrings #-}

module MetaInfoFile
  ( readTorrentFile
  , TorrentMeta(..)
  , MetaInfo(..)
  , MetaFile(..)
  , extractTorrentMeta
  , extractMetaInfo
  , extractMetaFile
  ) where

import Prelude hiding (readFile)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.ByteString (ByteString, readFile)
import Data.Maybe (mapMaybe)

import BType (BType(BDict, BInteger, BList, BString), fromBType)
import Bencode (decode)
import Util (liftEither)

data TorrentMeta = TorrentMeta
  { torrentMetaAnnounce :: ByteString
  , torrentMetaInfo :: MetaInfo
  } deriving (Show, Eq)

data MetaInfo = MetaInfo
  { metaInfoName :: ByteString
  , metaInfoPieceLength :: Integer
  , metaInfoPieces :: ByteString
  , metaInfoLength :: Maybe Integer
  , metaInfoFiles :: Maybe [MetaFile]
  } deriving (Show, Eq)

data MetaFile = MetaFile
  { metaFileLength :: Integer
  , metaFilePath :: [ByteString]
  } deriving (Show, Eq)

readTorrentFile :: FilePath -> ExceptT String IO TorrentMeta
readTorrentFile path = do
  fileContents <- liftIO $ readFile path
  dict <- liftEither $ decode fileContents
  case extractTorrentMeta dict of
    Nothing -> throwE "Could not unpack torrent file BType"
    Just result -> return result

extractTorrentMeta :: BType -> Maybe TorrentMeta
extractTorrentMeta (BDict dict) = do
  info <- lookup "info" dict
  (BString announce) <- lookup "announce" dict
  metaInfo <- extractMetaInfo info
  return
    TorrentMeta {torrentMetaAnnounce = announce, torrentMetaInfo = metaInfo}
extractTorrentMeta _ = error "Torrent meta must be a dictionary"

extractMetaInfo :: BType -> Maybe MetaInfo
extractMetaInfo (BDict dict) = do
  (BString name) <- lookup "name" dict
  (BInteger pieceLength) <- lookup "piece length" dict
  (BString pieces) <- lookup "pieces" dict
  let metaInfos =
        case lookup "files" dict of
          Nothing -> Nothing
          Just (BList files) -> Just (mapMaybe extractMetaFile files)
          _ -> error "Metainfos must be a list of dictionaries"
  Just
    MetaInfo
    { metaInfoName = name
    , metaInfoPieceLength = pieceLength
    , metaInfoPieces = pieces
    , metaInfoLength = fmap fromBType (lookup "length" dict)
    , metaInfoFiles = metaInfos
    }
extractMetaInfo _ = error "Metainfo must be a dictionary"

extractMetaFile :: BType -> Maybe MetaFile
extractMetaFile (BDict dict) = do
  (BInteger len) <- lookup "length" dict
  path <- lookup "path" dict
  return MetaFile {metaFileLength = len, metaFilePath = fromBType path}
extractMetaFile _ = error "Meta file must be a dictionary"
