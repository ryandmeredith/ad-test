module MNum (MNum (..)) where

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (Identity))
import Prelude as P

class Monad m => MNum m a where
  (+) :: a -> a -> m a
  (-) :: a -> a -> m a
  fromInteger :: Integer -> m a

instance Num a => MNum Identity a where
  (+) = coerce $ (P.+) @a
  (-) = coerce $ (P.-) @a
  fromInteger = coerce $ P.fromInteger @a
