module Util
    ( dropUntil
      ) where

import Data.Text (Text, dropWhile, drop)
import Prelude hiding (dropWhile, drop)

-- | This method must be specific to the Text datatype because
-- the Text interface does not match that of a list.
dropUntil :: (Char -> Bool) -> Text -> Text
dropUntil f xs = drop 1 $ dropWhile (not . f) xs
