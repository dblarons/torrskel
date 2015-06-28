{-# LANGUAGE FlexibleInstances #-}
module Bencode
    ( Encode,
      encode,
      decode,
      BType(BInteger, BString, BList, BDict)
      ) where

data BType
    = BString   String
    | BList    [BType]
    | BInteger  Integer
    | BDict     [(String, BType)]
    deriving (Eq, Show)

class Encode a where
    -- |Given a type that can be Bencoded, invoke the correct encoding
    -- function.
    encode :: a -> String
    -- |Given a String, parse the string into the resulting set of Bencode
    -- values.
    decode :: String -> a

instance Encode BType where
    -- |Encoding integers.
    encode (BInteger i) = "i" ++ show i ++ "e"

    -- |Encoding strings.
    encode (BString []) = "0:"
    encode (BString xs) = show (length xs) ++ ":" ++ xs

    -- |Encoding lists.
    encode (BList []) = "le"
    encode (BList xs) = "l" ++ foldl (\a x -> a ++ encode x) "" xs ++ "e"

    -- |Encoding dictionaries.
    encode (BDict xs) = "d" ++ foldl (\a x -> a ++ encode (BString $ fst x) ++ encode (snd x)) "" xs ++ "e"

    -- |Decoding integers.
    decode s = BInteger 5
