module Main (main) where

import Data.Bifunctor (Bifunctor (second))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Numeric.AD (grad')
import System.Environment (getArgs)

fib :: (Ord a, Num a) => a -> a
fib n
  | n <= 1 = n
  | otherwise = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  [ns] <- getArgs
  n <- readIO @Int ns
  print $ second runIdentity $ grad' (fib . runIdentity) $ Identity n
