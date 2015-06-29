{-# LANGUAGE FlexibleInstances #-}
module Bencode
    ( Encode,
      encode,
      decode,
      BError,
      BType(BInteger, BString, BList, BDict)
      ) where

import Util

type BError = String

data BType
    = BString   String
    | BList     [BType]
    | BInteger  Integer
    | BDict     [(String, BType)]
    deriving (Eq, Show)

-- |Type class for producing BEncoded strings.
class Encode a where
    -- |Given a type that can be Bencoded, invoke the correct encoding
    -- function.
    encode :: a -> String

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
    encode (BDict xs) = "d" ++ foldDict ++ "e"
        where foldDict = foldl (\a x -> a ++ encode (BString $ fst x) ++ encode (snd x)) "" xs

-- |Decode a Bencoded string completely and return the BType structure from
-- that string. Calls the recursive decoding function, rDecode, internally.
decode :: String -> Either BError BType
decode s = let val = rDecode s
           in case val of
                Left msg -> Left msg
                Right (v, rest) -> if rest /= ""
                                     then Left "String was not entirely decoded."
                                     else Right v

-- |Given a String, parse the string into the resulting set of Bencode
-- values and the remaining string.
rDecode :: String -> Either BError (BType, String)
rDecode [] = Left "No value found to decode."
rDecode s@(x:xs)
    | x == 'i' = case parseInt s of
                   Left msg -> Left msg
                   Right val -> Right (BInteger val, dropInt s)
    | x == 'l' = parseList xs []
    | x == 'd' = parseDict xs []
    | otherwise = case parseString s of
                    Left msg -> Left msg
                    Right val -> Right (BString val, dropString s (length val))

-- |Given a string and accumulator dictionary, return a dictionary
-- consisting of the accumulator and the new key/value pair. Also return
-- the rest of the string after the key/value pair has been extracted.
parseDict :: String -> [(String, BType)] -> Either BError (BType, String)
parseDict [] _ = Left "No value found to decode into dictionary."
parseDict s@(x:xs) acc
    | x == 'e' = Right (BDict acc, xs)
    | otherwise = let key = parseString s
                  in case key of
                       Left msg -> Left msg
                       Right k -> let val = rDecode $ dropString s (length k)
                                  in case val of
                                       Left msg -> Left msg
                                       Right (v, rest) -> parseDict rest (acc ++ [(k, v)])

-- |Given a string and accumulator list, return a list consisting of the
-- accumulator and the new key/value pair. Also return the rest of the
-- string after the next element of the list has been extracted.
parseList :: String -> [BType] -> Either BError (BType, String)
parseList [] _ = Left "No value found to decode into list."
parseList s@(x:xs) acc
    | x == 'e' = Right (BList acc, xs)
    | x == 'd' || x == 'l' = let val = rDecode s
                             in case val of
                                  Left msg -> Left msg
                                  Right (v, rest) -> parseList rest (acc ++ [v])
    | x == 'i' = let rest = dropInt s
                 in case parseInt s of
                      Left msg -> Left msg
                      Right val -> parseList rest (acc ++ [BInteger val])
    | otherwise = case parseString s of
                    Left msg -> Left msg
                    Right val -> let rest = dropString s (length val)
                                 in parseList rest (acc ++ [BString val])

-- |Parse a string from a Bencoded string.
parseString :: String -> Either BError String
parseString s = let maybeVal = readNumUntilChar s ':'
                    rest = dropUntil (== ':') s -- drop through ':'
                in case maybeVal of
                     Nothing -> Left "Invalid characters in decoded string length."
                     Just len -> Right $ take len rest

-- |Drop the string of length n from the Bencoded value.
dropString :: String -> Int -> String
dropString s n = drop n $ dropUntil (== ':') s

-- |Parse an integer from a Bencoded string.
parseInt :: String -> Either BError Integer
parseInt [] = Left "No value found to decode into string."
parseInt (_:xs) =
    let maybeVal = readNumUntilChar xs 'e'
    in case maybeVal of
         Nothing -> Left "Invalid characters in decoded integer."
         Just i -> Right $ toInteger i

-- |Drop an integer from a Bencoded string.
dropInt :: String -> String
dropInt = dropUntil (== 'e')

-- |Read a number from a string until a specified character is reached.
readNumUntilChar :: String -> Char -> Maybe Int
readNumUntilChar s c = let val = reads (takeWhile (/= c) s) :: [(Int, String)]
                       in case val of
                            [(i, "")] -> Just i
                            _ -> Nothing

