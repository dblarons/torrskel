{-# LANGUAGE FlexibleInstances #-}

module Bencode
  ( encode
  , decode
  ) where

import Prelude hiding (drop, head, length, null, tail, take, takeWhile)

import Data.ByteString (ByteString, append, drop, length, null, tail, take)
import Data.ByteString.Char8 (head, pack, takeWhile, unpack)

import BType
import Util

-- Type class for producing BEncoded strings.
class Encode a where
  encode :: a -> ByteString

instance Encode BType where
  encode (BInteger i) = pack $ "i" ++ show i ++ "e"
  encode (BString xs) = pack (show (length xs)) `append` pack ":" `append` xs
  encode (BList []) = pack "le"
  encode (BList xs) =
    pack "l" `append` foldl (\a x -> a `append` encode x) (pack "") xs `append`
    pack "e"
  encode (BDict xs) = pack "d" `append` foldDict `append` pack "e"
    where
      foldDict =
        foldl
          (\a x -> a `append` encode (BString $ fst x) `append` encode (snd x))
          (pack "")
          xs

-- Decode a Bencoded string completely and return the BType structure from
-- that string. Calls the recursive decoding function, rDecode, internally.
decode :: ByteString -> Either String BType
decode s = do
  (v, rest) <- rDecode s
  if rest /= pack ""
    then fail "String was not entirely decoded."
    else return v

-- Given a String, parse the string into the resulting set of Bencode
-- values and the remaining string.
rDecode :: ByteString -> Either String (BType, ByteString)
rDecode xs
  | null xs = fail "No value found to decode."
  | otherwise =
    case head xs of
      'i' -> do
        val <- parseInt xs
        return (BInteger val, dropInt xs)
      'l' -> parseList (tail xs) []
      'd' -> parseDict (tail xs) []
      _ -> do
        val <- parseString xs
        return (BString val, dropString xs (length val))

-- Given a string and accumulator dictionary, return a dictionary
-- consisting of the accumulator and the new key/value pair. Also return
-- the rest of the string after the key/value pair has been extracted.
parseDict ::
     ByteString -> [(ByteString, BType)] -> Either String (BType, ByteString)
parseDict xs acc =
  if null xs
    then fail "No value found to decode into dictionary."
    else case head xs of
           'e' -> return (BDict acc, tail xs)
           _ -> do
             key <- parseString xs
             (val, rest) <- rDecode $ dropString xs (length key)
             parseDict rest (acc ++ [(key, val)])

-- Given a string and accumulator list, return a list consisting of the
-- accumulator and the new key/value pair. Also return the rest of the
-- string after the next element of the list has been extracted.
parseList :: ByteString -> [BType] -> Either String (BType, ByteString)
parseList xs acc =
  if null xs
    then fail "No value found to decode into list."
    else case head xs of
           'e' -> return (BList acc, tail xs)
           'd' -> do
             (val, rest) <- rDecode xs
             parseList rest (acc ++ [val])
           'l' -> do
             (val, rest) <- rDecode xs
             parseList rest (acc ++ [val])
           'i' -> do
             let rest = dropInt xs
             val <- parseInt xs
             parseList rest (acc ++ [BInteger val])
           _ -> do
             val <- parseString xs
             let rest = dropString xs (length val)
             parseList rest (acc ++ [BString val])

-- Parse a string from a Bencoded string.
parseString :: ByteString -> Either String ByteString
parseString s = do
  strLen <- readNumUntilChar s ':'
  let rest = dropUntil (== ':') s -- drop through ':'
  return $ take strLen rest

-- Drop the string of length n from the Bencoded value.
dropString :: ByteString -> Int -> ByteString
dropString s n = drop n $ dropUntil (== ':') s

-- Parse an integer from a Bencoded string.
parseInt :: ByteString -> Either String Integer
parseInt xs =
  if null xs
    then fail "No value found to decode into string."
    else do
      val <- readNumUntilChar (tail xs) 'e'
      return $ toInteger val

-- Drop an integer from a Bencoded string.
dropInt :: ByteString -> ByteString
dropInt = dropUntil (== 'e')

-- Read a number from a string until a specified character is reached.
readNumUntilChar :: ByteString -> Char -> Either String Int
readNumUntilChar s c =
  let val = reads (unpack $ takeWhile (/= c) s) :: [(Int, String)]
  in case val of
       [(i, "")] -> return i
       _ -> fail "Invalid characters in decoded string."
