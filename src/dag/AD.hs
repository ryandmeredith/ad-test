{-# LANGUAGE TemplateHaskellQuotes #-}

module AD (D, autodiff) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Map
import Language.Haskell.TH
import Prelude hiding (lookup)

data BiMap = MkB (Map Name ExpQ) (Map Exp Name)

newtype D = MkD (StateT BiMap Q Name)

hashcons :: ExpQ -> StateT BiMap Q Name
hashcons e = do
  MkB m1 m2 <- get
  e' <- lift e
  case lookup e' m2 of
    Just n -> pure n
    Nothing -> do
      n <- lift $ newName "x"
      let m1' = insert n e m1
          m2' = insert e' n m2
      put $ MkB m1' m2'
      pure n

lift1 :: Name -> D -> D
lift1 f (MkD xs) = MkD $ do
  x <- xs
  hashcons $ appE (varE f) (varE x)

lift2 :: Name -> D -> D -> D
lift2 f (MkD xs) (MkD ys) = MkD $ do
  x <- xs
  y <- ys
  hashcons $ uInfixE (varE x) (varE f) (varE y)

instance Num D where
  (+) = lift2 '(+)
  (*) = lift2 '(*)
  (-) = lift2 '(-)
  negate = lift1 'negate
  abs = lift1 'abs
  signum = lift1 'signum
  fromInteger = MkD . hashcons . litE . integerL

autodiff :: (D -> D) -> ExpQ
autodiff f = do
  x <- newName "x"
  let MkD s = f $ MkD $ pure x
  (y, MkB m _) <- runStateT s $ MkB empty empty
  let decs = foldrWithKey (\n e xs -> valD (varP n) (normalB e) [] : xs) [] m
  lam1E (varP x) $ letE decs $ varE y
