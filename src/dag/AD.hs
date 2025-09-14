{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module AD (D, autodiff) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (..))
import Data.Map (empty, insert, lookup)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Language.Haskell.TH (
  Body (NormalB),
  Dec (ValD),
  Exp (AppE, AppTypeE, DoE, LamE, LitE, TupE, UInfixE, VarE),
  ExpQ,
  Lit (IntegerL),
  Name,
  Pat (SigP, VarP),
  Q,
  Stmt (BindS, LetS, NoBindS),
  Type (VarT),
  newName,
  varE,
 )
import Language.Haskell.TH.Syntax (getQ, putQ)
import Prelude hiding (lookup)

newtype D = MkD {runD :: ContT (Name, [Stmt] -> [Stmt]) Q (Name, Name)}

hashcons :: Exp -> (Name -> [Stmt]) -> ContT (Name, [Stmt] -> [Stmt]) Q (Name, Name)
hashcons e back = do
  Just m <- lift getQ
  case lookup e m of
    Just ns -> pure ns
    Nothing -> ContT $ \k -> do
      n <- newName "x"
      n' <- newName "x'"
      let m' = insert e (n, n') m
          f' =
            [ LetS [ValD (VarP n) (NormalB e) []]
            , BindS (VarP n') $ AppE (VarE 'newSTRef) $ LitE $ IntegerL 0
            ]
      putQ m'
      (r, dlist) <- k (n, n')
      pure (r, (<>) f' . dlist . (<>) (back n'))

lift1 :: Name -> (ExpQ -> ExpQ -> ExpQ) -> D -> D
lift1 f f' xd = MkD $ do
  (x, x') <- runD xd
  y' <- lift $ newName "y'"
  deriv <- lift $ f' (varE x) (varE y')
  hashcons (AppE (VarE f) $ VarE x) $ \n' ->
    [ BindS (VarP y') $ AppE (VarE 'readSTRef) $ VarE n'
    , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE x') deriv
    ]

lift2 :: Name -> (ExpQ -> ExpQ -> ExpQ -> ExpQ) -> (ExpQ -> ExpQ -> ExpQ -> ExpQ) -> D -> D -> D
lift2 f f1' f2' xd yd = MkD $ do
  (x, x') <- runD xd
  (y, y') <- runD yd
  z' <- lift $ newName "z'"
  derivX <- lift $ f1' (varE x) (varE y) (varE z')
  derivY <- lift $ f2' (varE x) (varE y) (varE z')
  hashcons (UInfixE (VarE x) (VarE f) (VarE y)) $ \n' ->
    [ BindS (VarP z') $ AppE (VarE 'readSTRef) $ VarE n'
    , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE x') derivX
    , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE y') derivY
    ]

instance Num D where
  (+) = lift2 '(+) (\_ _ z' -> [|(+ $z')|]) (\_ _ z' -> [|(+ $z')|])
  (-) = lift2 '(-) (\_ _ z' -> [|(+ $z')|]) (\_ _ z' -> [|(- $z')|])
  (*) = lift2 '(*) (\_ y z' -> [|(+ $z' * $y)|]) (\x _ z' -> [|(+ $z' * $x)|])
  negate = lift1 'negate $ \_ y' -> [|(- $y')|]
  abs = lift1 'abs $ \x y' -> [|(+ $y' * signum $x)|]
  signum (MkD xm) = MkD $ xm >>= (`hashcons` const []) . AppE (VarE 'signum) . VarE . fst
  fromInteger n = MkD $ hashcons (LitE $ IntegerL n) $ const []

autodiff :: (D -> D) -> ExpQ
autodiff f = do
  x <- newName "x"
  x' <- newName "x'"
  a <- newName "a"
  putQ $ empty @Exp @(Name, Name)
  (y, dlist) <- runContT (runD $ f $ MkD $ pure (x, x')) $ \(n, n') ->
    pure (n, (NoBindS (AppE (AppE (VarE 'writeSTRef) $ VarE n') $ LitE $ IntegerL 1) :))
  let new = BindS (VarP x') $ AppE (AppTypeE (VarE 'newSTRef) $ VarT a) $ LitE $ IntegerL 0
      ret = NoBindS $ UInfixE (TupE [Just $ VarE y, Nothing]) (VarE '(<$>)) $ AppE (VarE 'readSTRef) $ VarE x'
  pure $ LamE [SigP (VarP x) $ VarT a] $ DoE Nothing $ new : dlist [ret]
