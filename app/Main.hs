{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except
import Network.Wreq

import Bencode
import MetaInfoFile

main :: IO ()
main = do
  contents <- runExceptT $ readTorrentFile "bin/test.torrent"
  case contents of
    Left msg -> putStrLn msg
    Right val -> print val
