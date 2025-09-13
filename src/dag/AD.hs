{-# LANGUAGE TemplateHaskellQuotes #-}

module AD (D, autodiff) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Data.Map (Map, empty, insert, lookup)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Language.Haskell.TH (
  Body (NormalB),
  Dec (ValD),
  Exp (AppE, AppTypeE, DoE, InfixE, LamE, LitE, TupE, UInfixE, VarE),
  ExpQ,
  Lit (IntegerL),
  Name,
  Pat (SigP, VarP),
  Q,
  Stmt (BindS, LetS, NoBindS),
  Type (VarT),
  newName,
 )
import Prelude hiding (lookup)

data DAG = MkB ([Stmt] -> [Stmt]) (Stmt -> [Stmt]) (Map Exp (Name, Name))

newtype D = MkD (StateT DAG Q (Name, Name))

hashcons :: Exp -> (Name -> [Stmt]) -> StateT DAG Q (Name, Name)
hashcons e back = do
  MkB f b m <- get
  case lookup e m of
    Just ns -> pure ns
    Nothing -> do
      n <- lift $ newName "x"
      n' <- lift $ newName "x'"
      let m' = insert e (n, n') m
          f' =
            [ LetS [ValD (VarP n) (NormalB e) []]
            , BindS (VarP n') $ AppE (VarE 'newSTRef) $ LitE $ IntegerL 0
            ]
      put $ MkB (f . (<>) f') ((back n' <>) . b) m'
      pure (n, n')

lift1 :: Name -> (Exp -> Exp -> Exp) -> D -> D
lift1 f f' (MkD xm) = MkD $ do
  (x, x') <- xm
  y' <- lift $ newName "y'"
  hashcons (AppE (VarE f) $ VarE x) $ \n' ->
    [ BindS (VarP y') $ AppE (VarE 'readSTRef) $ VarE n'
    , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE x') $ f' (VarE x) (VarE y')
    ]

lift2 :: Name -> (Exp -> Exp -> Exp -> Exp) -> (Exp -> Exp -> Exp -> Exp) -> D -> D -> D
lift2 f f1' f2' (MkD xm) (MkD ym) = MkD $ do
  (x, x') <- xm
  (y, y') <- ym
  z' <- lift $ newName "z'"
  hashcons (UInfixE (VarE x) (VarE f) (VarE y)) $ \n' ->
    [ BindS (VarP z') $ AppE (VarE 'readSTRef) $ VarE n'
    , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE x') $ f1' (VarE x) (VarE y) (VarE z')
    , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE y') $ f2' (VarE x) (VarE y) (VarE z')
    ]

instance Num D where
  (+) =
    lift2
      '(+)
      (\_ _ z' -> InfixE Nothing (VarE '(+)) $ Just z')
      (\_ _ z' -> InfixE Nothing (VarE '(+)) $ Just z')
  (-) =
    lift2
      '(-)
      (\_ _ z' -> InfixE Nothing (VarE '(+)) $ Just z')
      (\_ _ z' -> InfixE Nothing (VarE '(-)) $ Just z')
  (*) =
    lift2
      '(*)
      (\_ y z' -> InfixE Nothing (VarE '(+)) $ Just $ UInfixE z' (VarE '(*)) y)
      (\x _ z' -> InfixE Nothing (VarE '(+)) $ Just $ UInfixE z' (VarE '(*)) x)
  negate = lift1 'negate $ \_ y' -> InfixE Nothing (VarE '(-)) $ Just y'
  abs = lift1 'abs $ \x y' ->
    InfixE Nothing (VarE '(+)) $ Just $ UInfixE y' (VarE '(*)) $ AppE (VarE 'signum) x
  signum (MkD xs) = MkD $ xs >>= (`hashcons` const []) . AppE (VarE 'signum) . VarE . fst
  fromInteger n = MkD $ hashcons (LitE $ IntegerL n) $ const []

autodiff :: (D -> D) -> ExpQ
autodiff f = do
  x <- newName "x"
  x' <- newName "x'"
  a <- newName "a"
  let MkD m = f $ MkD $ pure (x, x')
  ((y, y'), MkB forward backward _) <- runStateT m $ MkB id (: []) empty
  let new = BindS (VarP x') $ AppE (AppTypeE (VarE 'newSTRef) $ VarT a) $ LitE $ IntegerL 0
      write = NoBindS $ AppE (AppE (VarE 'writeSTRef) $ VarE y') $ LitE $ IntegerL 1
      ret = NoBindS $ UInfixE (TupE [Just $ VarE y, Nothing]) (VarE '(<$>)) $ AppE (VarE 'readSTRef) $ VarE x'
  pure $ LamE [SigP (VarP x) $ VarT a] $ DoE Nothing $ new : forward (write : backward ret)
