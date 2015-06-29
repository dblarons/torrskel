module Util
    ( dropUntil
      ) where

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f xs = drop 1 $ dropWhile (not . f) xs
