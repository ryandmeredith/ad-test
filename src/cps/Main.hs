module Main (main) where

import AD (autodiff)
import Control.Monad.ST (stToIO)
import MNum (MNum (..))
import System.Environment (getArgs)
import Prelude hiding (fromInteger, (+), (-))

{- HLINT ignore fib "Redundant fromInteger" -}
fib :: (MNum m a, Ord a) => a -> m a
fib n0 = do
  one <- fromInteger 1
  if n0 <= one
    then pure n0
    else do
      n1 <- n0 - one
      n2 <- n1 - one
      x <- fib n1
      y <- fib n2
      x + y

main :: IO ()
main = do
  [ns] <- getArgs
  n <- readIO @Int ns
  x <- stToIO $ autodiff fib n
  print x
