module AD (M, D, autodiff) where

import Control.Monad (ap)
import Control.Monad.ST (ST)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import MNum (MNum)
import MNum qualified

newtype M s a = MkM {runM :: forall r. (a -> ST s r) -> ST s r} deriving Functor

instance Applicative (M s) where
  pure x = MkM ($ x)
  (<*>) = ap

instance Monad (M s) where
  MkM m >>= f = MkM $ \k -> m $ \x -> runM (f x) k

data D s a = MkD !a {-# UNPACK #-} !(STRef s a)

{-# INLINE lift #-}
lift :: Num a => a -> M s (D s a)
lift x = MkM $ \k -> newSTRef 0 >>= k . MkD x

{-# INLINE lift2 #-}
lift2 ::
  Num c =>
  (a -> b -> c) ->
  (a -> b -> c -> a -> a) ->
  (a -> b -> c -> b -> b) ->
  D s a ->
  D s b ->
  M s (D s c)
lift2 f f1' f2' (MkD x x') (MkD y y') = MkM $ \k -> do
  r <- newSTRef 0
  z <- k $ MkD (f x y) r
  z' <- readSTRef r
  modifySTRef' x' $ f1' x y z'
  modifySTRef' y' $ f2' x y z'
  pure z

{-# INLINE project #-}
project :: (a -> b -> c) -> D s a -> D s b -> c
project f (MkD x _) (MkD y _) = f x y

instance Num a => MNum (M s) (D s a) where
  (+) = lift2 (+) (\_ _ -> (+)) (\_ _ -> (+))
  (-) = lift2 (-) (\_ _ -> (+)) (\_ _ -> subtract)
  fromInteger n = lift $ fromInteger n

instance Eq a => Eq (D s a) where
  (==) = project (==)

instance Ord a => Ord (D s a) where
  (<=) = project (<=)

{-# INLINABLE autodiff #-}
autodiff :: (Num a, Num b) => (D s a -> M s (D s b)) -> a -> ST s (b, a)
autodiff f x = do
  r <- newSTRef 0
  y <- runM (f $ MkD x r) $ \(MkD y y') -> y <$ writeSTRef y' 1
  x' <- readSTRef r
  pure (y, x')
