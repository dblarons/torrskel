{-# LANGUAGE OverloadedStrings #-}

module FileManager
    ( decodeFile
      ) where

import Data.Text()
import qualified Data.Text.IO as T

import Bencode

decodeFile :: FilePath -> IO ()
decodeFile path = do contents <- T.readFile path
                     case decode contents of
                       Left msg -> putStrLn msg
                       Right val -> putStrLn "Got val"
