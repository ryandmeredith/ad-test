{-# LANGUAGE LexicalNegation #-}

module AD (D, autodiff) where

import Control.Category
import Lin
import Prelude hiding (id)

data D a b = MkD b (Lin b a)

instance (Num a, Num b) => Num (D a b) where
  MkD x x' + MkD y y' = MkD (x + y) $ plus x' y'
  MkD x x' - MkD y y' = MkD (x - y) $ minus x' y'
  MkD x x' * MkD y y' = MkD (x * y) $ plus (scale x y') (scale y x')
  negate (MkD x x') = MkD -x $ neg x'
  abs (MkD x x') = MkD (abs x) $ scale (signum x) x'
  signum (MkD x _) = MkD (signum x) zero
  fromInteger n = MkD (fromInteger n) zero

instance Eq b => Eq (D a b) where
  MkD x _ == MkD y _ = x == y

instance Ord b => Ord (D a b) where
  MkD x _ <= MkD y _ = x <= y

{-# INLINEABLE autodiff #-}
autodiff :: Num b => (D a a -> D a b) -> a -> (b, a)
autodiff f x = case f $ MkD x id of
  MkD y y' -> (y, app y' 1)
