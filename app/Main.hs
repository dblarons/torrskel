module Main where

import MetaInfoFile
import Control.Monad.Trans.Except
import Bencode

main :: IO ()
main = do
    contents <- runExceptT $ readTorrentFile "bin/test.torrent"
    case contents of
      Left msg -> putStrLn msg
      Right val -> print val
