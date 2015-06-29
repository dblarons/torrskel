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

decode :: String -> Either BError BType
decode s = let val = rDecode s
           in case val of
                Left msg -> Left msg
                Right v -> let (result, rest) = v
                           in if rest /= ""
                                then Left "String was not entirely decoded."
                                else Right result

-- |Given a String, parse the string into the resulting set of Bencode
-- values.
rDecode :: String -> Either BError (BType, String)
rDecode s@(x:xs)
    | x == 'i' = case parseInt s of
                   Left msg -> Left msg
                   Right val -> Right (BInteger val, dropInt s)
    | x == 'l' = parseList xs []
    | x == 'd' = parseDict xs []
    | otherwise = case parseString s of
                    Left msg -> Left msg
                    Right val -> Right (BString val, dropString s (length val))

parseDict :: String -> [(String, BType)] -> Either String (BType, String)
parseDict s@(x:xs) acc
    | x == 'e' = Right (BDict acc, xs)
    | otherwise = let key = parseString s
                  in case key of
                       Left msg -> Left msg
                       Right k -> let val = rDecode $ dropString s (length k)
                                  in case val of
                                       Left msg -> Left msg
                                       Right v -> let (value, rest) = v
                                                  in parseDict rest (acc ++ [(k, value)])

parseList :: String -> [BType] -> Either String (BType, String)
parseList s@(x:xs) acc
    | x == 'e' = Right (BList acc, xs)
    | x == 'd' || x == 'l' = let val = rDecode s
                             in case val of
                                  Left msg -> Left msg
                                  Right val -> let (v, rest) = val
                                               in parseList rest (acc ++ [v])
    | x == 'i' = let rest = dropInt s
                 in case parseInt s of
                      Left msg -> Left msg
                      Right val -> parseList rest (acc ++ [BInteger val])
    | otherwise = case parseString s of
                    Left msg -> Left msg
                    Right val -> let rest = dropString s (length val)
                                 in parseList rest (acc ++ [BString val])

-- |Parse a string from a Bencoded string and return it, along with the
-- remaining portion of the original Bencoded string.
parseString :: String -> Either BError String
parseString s = let maybeVal = readNumUntilChar s ':'
                    rest = dropUntil (== ':') s -- drop through ':'
                in case maybeVal of
                     Nothing -> Left "Invalid characters in decoded string length."
                     Just len -> Right $ take len rest

-- |Drop n the string of length n from the Bencoded value.
dropString :: String -> Int -> String
dropString s n = drop n $ dropUntil (== ':') s

-- |Parse an integer from a Bencoded string and return it, along with the
-- remaining portion of the original Bencoded string.
parseInt :: String -> Either BError Integer
parseInt (x:xs) =
    let maybeVal = readNumUntilChar xs 'e'
    in case maybeVal of
         Nothing -> Left "Invalid characters in decoded integer."
         Just i -> Right $ toInteger i

dropInt :: String -> String
dropInt = dropUntil (== 'e')

readNumUntilChar :: String -> Char -> Maybe Int
readNumUntilChar s c = let val = reads (takeWhile (/= c) s) :: [(Int, String)]
                       in case val of
                            [(i, "")] -> Just i
                            otherwise -> Nothing

