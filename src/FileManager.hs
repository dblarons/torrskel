{-# LANGUAGE OverloadedStrings #-}

module FileManager
    ( decodeFile
      ) where

import Data.ByteString()
import qualified Data.ByteString as B

import Bencode

decodeFile :: FilePath -> IO ()
decodeFile path = do contents <- B.readFile path
                     case decode contents of
                       Left msg -> putStrLn msg
                       Right val -> putStrLn "Got val"
