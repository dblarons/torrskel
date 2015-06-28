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
    decode :: String -> Either String a

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

    -- |Decoding strings into BTypes.
    decode = decode'
        where decode' acc a = case decodeVal acc a of
                                Left msg -> Left msg
                                Right val -> Right val
              decodeVal acc s@(x:xs)
                    | x == 'i' = Right $ BInteger 10
                    | x == 'l' = Right $ BList []
                    | x == 'd' = Right $ BDict []
                    | otherwise = Right $ BString "foo"

-- |Parse a string from a Bencoded string and return it, along with the
-- remaining portion of the original Bencoded string.
parseString :: String -> (String, String)
parseString s = let len = read (takeWhile (/= ':') s) :: Int
                    rest = drop 1 $ dropWhile (/= ':') s -- drop through ':'
                in parseString' len rest
    where parseString' len rest = (take len rest, drop len rest)

-- |Parse an integer from a Bencoded string and return it, along with the
-- remaining portion of the original Bencoded string.
parseInt :: String -> Either String (Integer, String)
parseInt xs =
    let parsed = reads (takeWhile (/= 'e') xs) :: [(Int, String)]
    -- |Integer must contain only numbers.
    in case parsed of
         [] -> Left "Invalid characters in decoded integer."
         [(i, s)] -> let rest = drop (i + 1) xs -- drop through 'e'
                     in if not $ null s
                          then Left "Invalid characters in decoded integer."
                          else Right (toInteger i, rest)
