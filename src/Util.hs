module Util
  ( dropUntil
  , liftEither
  ) where

import Prelude hiding (drop, dropWhile)

import Control.Monad.Trans.Except
import Data.ByteString.Char8 (ByteString, drop, dropWhile)

-- This method must be specific to the ByteString datatype because
-- the ByteString interface does not match that of a list.
dropUntil :: (Char -> Bool) -> ByteString -> ByteString
dropUntil f xs = drop 1 $ dropWhile (not . f) xs

liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither x = ExceptT $ return x
