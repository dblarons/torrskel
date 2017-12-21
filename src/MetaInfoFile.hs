module MetaInfoFile
  ( readTorrentFile
  , TorrentMeta(..)
  , MetaInfo(..)
  , MetaFile(..)
  , extractTorrentMeta
  , extractMetaInfo
  , extractMetaFile
  ) where

import Data.ByteString (ByteString, readFile)
import Data.ByteString.Char8 (pack)

import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Except
import Prelude hiding (readFile)

import BType
  ( BType(BList)
  , unwrapBDict
  , unwrapBInteger
  , unwrapBString
  , unwrapBStringList
  )
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
  contents <- liftIO $ readFile path
  val <- liftEither $ decode contents
  case extractTorrentMeta val of
    Nothing -> throwE "Unable to decode torrent meta file."
    Just v -> return v

extractTorrentMeta :: BType -> Maybe TorrentMeta
extractTorrentMeta dict = do
  d <- unwrapBDict dict
  info <- lookup (pack "info") d
  metaInfo <- extractMetaInfo info
  announce <- lookup (pack "announce") d >>= unwrapBString
  Just TorrentMeta {torrentMetaAnnounce = announce, torrentMetaInfo = metaInfo}

extractMetaInfo :: BType -> Maybe MetaInfo
extractMetaInfo dict = do
  d <- unwrapBDict dict
  name <- lookup (pack "name") d >>= unwrapBString
  pieceLength <- lookup (pack "piece length") d >>= unwrapBInteger
  pieces <- lookup (pack "pieces") d >>= unwrapBString
  let len =
        case lookup (pack "length") d of
          Nothing -> Nothing
          Just l -> unwrapBInteger l
      files =
        case lookup (pack "files") d of
          Nothing -> Nothing
          Just fs -> extractMetaFilesRecurse fs []
  Just
    MetaInfo
    { metaInfoName = name
    , metaInfoPieceLength = pieceLength
    , metaInfoPieces = pieces
    , metaInfoLength = len
    , metaInfoFiles = files
    }

extractMetaFilesRecurse :: BType -> [MetaFile] -> Maybe [MetaFile]
extractMetaFilesRecurse (BList []) l = Just l
extractMetaFilesRecurse (BList (x:xs)) l = do
  metaFile <- extractMetaFile x
  extractMetaFilesRecurse (BList xs) (l ++ [metaFile])
extractMetaFilesRecurse _ _ = Nothing

extractMetaFile :: BType -> Maybe MetaFile
extractMetaFile dict = do
  d <- unwrapBDict dict
  l <- lookup (pack "length") d >>= unwrapBInteger
  path <- lookup (pack "path") d >>= unwrapBStringList
  Just MetaFile {metaFileLength = l, metaFilePath = path}
