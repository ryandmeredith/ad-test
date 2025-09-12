{-# LANGUAGE TemplateHaskellQuotes #-}

module AD (D, autodiff) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Map
import Language.Haskell.TH
import Prelude hiding (lookup)

data BiMap = MkB (Map Name Exp) (Map Exp Name)

newtype D = MkD (StateT BiMap Q Name)

hashcons :: Exp -> StateT BiMap Q Name
hashcons e = do
  MkB m1 m2 <- get
  case lookup e m2 of
    Just n -> pure n
    Nothing -> do
      n <- lift $ newName "x"
      let m1' = insert n e m1
          m2' = insert e n m2
      put $ MkB m1' m2'
      pure n

lift1 :: Name -> D -> D
lift1 f (MkD xs) = MkD $ do
  x <- xs
  hashcons $ AppE (VarE f) (VarE x)

lift2 :: Name -> D -> D -> D
lift2 f (MkD xs) (MkD ys) = MkD $ do
  x <- xs
  y <- ys
  hashcons $ UInfixE (VarE x) (VarE f) (VarE y)

instance Num D where
  (+) = lift2 '(+)
  (*) = lift2 '(*)
  (-) = lift2 '(-)
  negate = lift1 'negate
  abs = lift1 'abs
  signum = lift1 'signum
  fromInteger = MkD . hashcons . LitE . IntegerL

autodiff :: (D -> D) -> ExpQ
autodiff f = do
  x <- newName "x"
  let MkD s = f $ MkD $ pure x
  (y, MkB m _) <- runStateT s $ MkB empty empty
  let decs = foldrWithKey (\n e xs -> ValD (VarP n) (NormalB e) [] : xs) [] m
  pure $ LamE [VarP x] $ LetE decs $ VarE y
