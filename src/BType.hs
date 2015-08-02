module BType
    ( BType(BInteger, BString, BList, BDict)
    , unwrapBString
    , unwrapBInteger
    , unwrapBDict
    , unwrapBStringList
    ) where

import Data.ByteString (ByteString)

data BType
    = BString   ByteString
    | BList     [BType]
    | BInteger  Integer
    | BDict     [(ByteString, BType)]
    deriving (Eq, Show)

-- | What follows is a series of unfortunate functions that are currently
-- required to unwrap by BType data types. All return values are wrapped
-- in `Maybe` so as to avoid creating a set of partial functions. I was not
-- able to find a generic solution to the unwrapping problem; thus, I was
-- left with my current approach.

unwrapBString :: BType -> Maybe ByteString
unwrapBString (BString s) = Just s
unwrapBString _ = Nothing

unwrapBInteger :: BType -> Maybe Integer
unwrapBInteger (BInteger i) = Just i
unwrapBInteger _ = Nothing

unwrapBDict :: BType -> Maybe [(ByteString, BType)]
unwrapBDict (BDict d) = Just d
unwrapBDict _ = Nothing

unwrapBStringList :: BType -> Maybe [ByteString]
unwrapBStringList bList = unwrapBStringListRecurse bList []

unwrapBStringListRecurse :: BType -> [ByteString] -> Maybe [ByteString]
unwrapBStringListRecurse (BList []) l = Just l
unwrapBStringListRecurse (BList (x:xs)) l = do
    next <- unwrapBString x
    unwrapBStringListRecurse (BList xs) (l ++ [next])
unwrapBStringListRecurse _ _ = Nothing
