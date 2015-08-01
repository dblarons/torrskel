module MetaInfoFile
    ( readTorrentFile
      ) where

import Data.ByteString (ByteString, readFile)
import Prelude hiding (readFile)

import Bencode

data TorrentMeta = TorrentMeta
    { torrentMetaAnnounce :: String
    , torrentMetaInfo     :: MetaInfo
    }

data MetaInfo = MetaInfo
    { metaInfoName        :: ByteString
    , metaInfoPieceLength :: Integer
    , metaInfoPieces      :: ByteString
    , metaInfoLength      :: Maybe Integer
    , metaInfoFiles       :: Maybe [MetaFilesList]
    }

data MetaFilesList = MetaFilesList
    { metaFilesListLength :: Integer
    , metaFilesListPath   :: [ByteString]
    }

readTorrentFile :: FilePath -> IO (Either String BType)
readTorrentFile path = do contents <- readFile path
                          return $ decode contents
