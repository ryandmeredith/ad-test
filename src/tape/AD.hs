module AD (D, autodiff) where

import Control.Monad (join)
import Data.IORef (IORef, modifyIORef, modifyIORef', newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafeDupablePerformIO)

{-# NOINLINE tape #-}
tape :: IORef (IO ())
tape = unsafeDupablePerformIO $ newIORef mempty

data D a = MkD !a {-# UNPACK #-} !(IORef a)

{-# INLINE lift #-}
lift :: Num a => a -> D a
lift x = unsafeDupablePerformIO $ MkD x <$> newIORef 0

{-# INLINE lift1 #-}
lift1 :: Num b => (a -> b) -> (a -> b -> a -> a) -> D a -> D b
lift1 f f' (MkD x x') = unsafeDupablePerformIO $ do
  r <- newIORef 0
  modifyIORef tape $ \backprop -> do
    y' <- readIORef r
    modifyIORef' x' $ f' x y'
    backprop
  pure $ MkD (f x) r

{-# INLINE lift2 #-}
lift2 ::
  Num c =>
  (a -> b -> c) ->
  (a -> b -> c -> a -> a) ->
  (a -> b -> c -> b -> b) ->
  D a ->
  D b ->
  D c
lift2 f f1' f2' (MkD x x') (MkD y y') = unsafeDupablePerformIO $ do
  r <- newIORef 0
  modifyIORef tape $ \backprop -> do
    z' <- readIORef r
    modifyIORef' x' $ f1' x y z'
    modifyIORef' y' $ f2' x y z'
    backprop
  pure $ MkD (f x y) r

{-# INLINE project #-}
project :: (a -> b -> c) -> D a -> D b -> c
project f (MkD x _) (MkD y _) = f x y

instance Num a => Num (D a) where
  (+) = lift2 (+) (\_ _ -> (+)) (\_ _ -> (+))
  (-) = lift2 (-) (\_ _ -> (+)) (\_ _ -> subtract)
  (*) = lift2 (*) (\_ y z' -> (+ z' * y)) (\x _ z' -> (+ z' * x))
  abs = lift1 abs $ \x y' -> (+ y' * signum x)
  signum (MkD x _) = lift $ signum x
  fromInteger n = lift $ fromInteger n

instance Eq a => Eq (D a) where
  (==) = project (==)

instance Ord a => Ord (D a) where
  (<=) = project (<=)

{-# INLINEABLE autodiff #-}
autodiff :: (Num a, Num b) => (D a -> D b) -> a -> IO (b, a)
autodiff f x = do
  r <- newIORef 0
  let MkD y y' = f $ MkD x r
  writeIORef y' 1
  join $ readIORef tape
  x' <- readIORef r
  pure (y, x')
