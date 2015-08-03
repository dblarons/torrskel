module Util
    ( dropUntil
    , liftEither
      ) where

import Data.ByteString.Char8 (ByteString, dropWhile, drop)
import Control.Monad.Trans.Except
import Prelude hiding (dropWhile, drop)

-- | This method must be specific to the Text datatype because
-- the Text interface does not match that of a list.
dropUntil :: (Char -> Bool) -> ByteString -> ByteString
dropUntil f xs = drop 1 $ dropWhile (not . f) xs

liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither x = ExceptT $ return x
