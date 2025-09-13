{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import AD (autodiff)
import Control.Monad.ST (stToIO)
import System.Environment (getArgs)

main :: IO ()
main = do
  [ns] <- getArgs
  n <- readIO @Integer ns
  (!_, !_) <- stToIO $ $(autodiff (^ (64 :: Int))) n
  pure ()
