{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Bencode
    ( Encode,
      encode,
      decode,
      BError,
      BType(BInteger, BString, BList, BDict)
      ) where

import Data.Text (Text, unpack, empty, length, append, head,
                 tail, null, take, drop, takeWhile)
import qualified Text.Show.Text as TS
import Prelude hiding (length, head, tail, null, take, drop, takeWhile)

import Util

type BError = String

data BType
    = BString   Text
    | BList     [BType]
    | BInteger  Integer
    | BDict     [(Text, BType)]
    deriving (Eq, Show)

-- |Type class for producing BEncoded strings.
class Encode a where
    -- |Given a type that can be Bencoded, invoke the correct encoding
    -- function.
    encode :: a -> Text

instance Encode BType where
    -- |Encoding integers.
    encode (BInteger i) = "i" `append` TS.show i `append` "e"

    -- |Encoding strings.
    encode (BString xs) = TS.show (length xs) `append` ":" `append` xs

    -- |Encoding lists.
    encode (BList []) = "le"
    encode (BList xs) = "l" `append` foldl (\a x -> a `append` encode x) "" xs `append` "e"

    -- |Encoding dictionaries.
    encode (BDict xs) = "d" `append` foldDict `append` "e"
        where foldDict = foldl (\a x -> a `append` encode (BString $ fst x) `append` encode (snd x)) "" xs

-- |Decode a Bencoded string completely and return the BType structure from
-- that string. Calls the recursive decoding function, rDecode, internally.
decode :: Text -> Either BError BType
decode s = do (v, rest) <- rDecode s
              if rest /= ""
                then Left "String was not entirely decoded."
                else Right v

-- |Given a String, parse the string into the resulting set of Bencode
-- values and the remaining string.
rDecode :: Text -> Either BError (BType, Text)
rDecode xs
    | null xs = Left "No value found to decode."
    | otherwise =
        case head xs of
          'i' -> do val <- parseInt xs
                    Right (BInteger val, dropInt xs)
          'l' -> parseList (tail xs) []
          'd' -> parseDict (tail xs) []
          _ -> do val <- parseString xs
                  Right (BString val, dropString xs (length val))

-- |Given a string and accumulator dictionary, return a dictionary
-- consisting of the accumulator and the new key/value pair. Also return
-- the rest of the string after the key/value pair has been extracted.
parseDict :: Text -> [(Text, BType)] -> Either BError (BType, Text)
parseDict xs acc =
    if null xs
      then Left "No value found to decode into dictionary."
      else case head xs of
         'e' -> Right (BDict acc, tail xs)
         _ ->  do key <- parseString xs
                  (val, rest) <- rDecode $ dropString xs (length key)
                  parseDict rest (acc ++ [(key, val)])

-- |Given a string and accumulator list, return a list consisting of the
-- accumulator and the new key/value pair. Also return the rest of the
-- string after the next element of the list has been extracted.
parseList :: Text -> [BType] -> Either BError (BType, Text)
parseList xs acc =
    if null xs
      then Left "No value found to decode into list."
      else case head xs of
        'e' -> Right (BList acc, tail xs)
        'd' -> do (val, rest) <- rDecode xs
                  parseList rest (acc ++ [val])
        'l' -> do (val, rest) <- rDecode xs
                  parseList rest (acc ++ [val])
        'i' -> do let rest = dropInt xs
                  val <- parseInt xs
                  parseList rest (acc ++ [BInteger val])
        _ ->  do val <- parseString xs
                 let rest = dropString xs (length val)
                 parseList rest (acc ++ [BString val])

-- |Parse a string from a Bencoded string.
parseString :: Text -> Either BError Text
parseString s = do strLen <- readNumUntilChar s ':'
                   let rest = dropUntil (== ':') s -- drop through ':'
                   Right $ take strLen rest

-- |Drop the string of length n from the Bencoded value.
dropString :: Text -> Int -> Text
dropString s n = drop n $ dropUntil (== ':') s

-- |Parse an integer from a Bencoded string.
parseInt :: Text -> Either BError Integer
parseInt xs = 
    if null xs
      then Left "No value found to decode into string."
      else do val <- readNumUntilChar (tail xs) 'e'
              Right $ toInteger val

-- |Drop an integer from a Bencoded string.
dropInt :: Text -> Text
dropInt = dropUntil (== 'e')

-- |Read a number from a string until a specified character is reached.
readNumUntilChar :: Text -> Char -> Either BError Int
readNumUntilChar s c = 
    let val = reads (unpack $ takeWhile (/= c) s) :: [(Int, String)]
    in case val of
         [(i, "")] -> Right i
         _ -> Left "Invalid characters in decoded string."

