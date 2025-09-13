{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module AD (D, autodiff) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Data.Map (Map, empty, insert, lookup)
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

lift1 :: Name -> (ExpQ -> ExpQ -> ExpQ) -> D -> D
lift1 f f' (MkD xm) = MkD $ do
  (x, x') <- xm
  y' <- lift $ newName "y'"
  deriv <- lift $ f' (varE x) (varE y')
  hashcons (AppE (VarE f) $ VarE x) $ \n' ->
    [ BindS (VarP y') $ AppE (VarE 'readSTRef) $ VarE n'
    , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE x') deriv
    ]

lift2 :: Name -> (ExpQ -> ExpQ -> ExpQ -> ExpQ) -> (ExpQ -> ExpQ -> ExpQ -> ExpQ) -> D -> D -> D
lift2 f f1' f2' (MkD xm) (MkD ym) = MkD $ do
  (x, x') <- xm
  (y, y') <- ym
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
  let MkD m = f $ MkD $ pure (x, x')
  ((y, y'), MkB forward backward _) <- runStateT m $ MkB id (: []) empty
  let new = BindS (VarP x') $ AppE (AppTypeE (VarE 'newSTRef) $ VarT a) $ LitE $ IntegerL 0
      write = NoBindS $ AppE (AppE (VarE 'writeSTRef) $ VarE y') $ LitE $ IntegerL 1
      ret = NoBindS $ UInfixE (TupE [Just $ VarE y, Nothing]) (VarE '(<$>)) $ AppE (VarE 'readSTRef) $ VarE x'
  pure $ LamE [SigP (VarP x) $ VarT a] $ DoE Nothing $ new : forward (write : backward ret)
