{-# LANGUAGE FlexibleInstances #-}

module BType
  ( BType(BInteger, BString, BList, BDict)
  , fromBType
  , BData
  ) where

import Data.ByteString (ByteString)

data BType
  = BString ByteString
  | BList [BType]
  | BInteger Integer
  | BDict [(ByteString, BType)]
  deriving (Eq, Show)

class BData a where
  fromBType :: BType -> a

instance BData ByteString where
  fromBType (BString s) = s
  fromBType _ = error "BString should only hold ByteString"

instance BData Integer where
  fromBType (BInteger i) = i
  fromBType _ = error "BInteger should only hold Integer"

instance BData [ByteString] where
  fromBType (BList xs) = map fromBType xs
  fromBType _ = error "BList should only contain ByteStrings (for now)"
