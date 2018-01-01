{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module MetaInfoFile
  ( readTorrentFile
  , TorrentMeta(..)
  , MetaInfo(..)
  , MetaFile(..)
  , unpack
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
  case unpack dict of
    Nothing -> throwE "Could not unpack torrent file BType"
    Just result -> return result

class Torrent a where
  unpack :: BType -> Maybe a

instance Torrent TorrentMeta where
  unpack (BDict dict) = do
    info <- lookup "info" dict
    (BString announce) <- lookup "announce" dict
    metaInfo <- unpack info
    return
      TorrentMeta {torrentMetaAnnounce = announce, torrentMetaInfo = metaInfo}
  unpack _ = error "Torrent meta must be a dictionary"

getSharedMetaInfoKeys ::
     [(ByteString, BType)] -> Maybe (ByteString, Integer, ByteString)
getSharedMetaInfoKeys dict = do
  (BString name) <- lookup "name" dict
  (BInteger pieceLength) <- lookup "piece length" dict
  (BString pieces) <- lookup "pieces" dict
  Just (name, pieceLength, pieces)

instance Torrent MetaInfo where
  unpack (BDict dict@(lookup "files" -> Just (BList files))) = do
    (name, pieceLength, pieces) <- getSharedMetaInfoKeys dict
    Just
      MetaInfo
      { metaInfoName = name
      , metaInfoPieceLength = pieceLength
      , metaInfoPieces = pieces
      , metaInfoLength = Nothing
      , metaInfoFiles = Just (mapMaybe unpack files)
      }
  unpack (BDict dict@(lookup "files" -> Nothing)) = do
    (name, pieceLength, pieces) <- getSharedMetaInfoKeys dict
    (BInteger len) <- lookup "length" dict
    Just
      MetaInfo
      { metaInfoName = name
      , metaInfoPieceLength = pieceLength
      , metaInfoPieces = pieces
      , metaInfoLength = Just len
      , metaInfoFiles = Nothing
      }
  unpack _ = error "Metainfo must be a dictionary"

instance Torrent MetaFile where
  unpack (BDict dict) = do
    (BInteger len) <- lookup "length" dict
    path <- lookup "path" dict
    return MetaFile {metaFileLength = len, metaFilePath = fromBType path}
  unpack _ = error "Meta file must be a dictionary"
