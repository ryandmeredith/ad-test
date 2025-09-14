{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module AD (D, autodiff) where

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
import Language.Haskell.TH.Syntax (getQ, putQ)
import Prelude hiding (lookup)

data DAG = MkB ([Stmt] -> [Stmt]) [Stmt] (Map Exp (Name, Name))

newtype D = MkD {runD :: Q (Name, Name)}

hashcons :: Exp -> (Name -> [Stmt]) -> Q (Name, Name)
hashcons e back = do
  Just (MkB f b m) <- getQ
  case lookup e m of
    Just ns -> pure ns
    Nothing -> do
      n <- newName "x"
      n' <- newName "x'"
      let m' = insert e (n, n') m
          f' =
            [ LetS [ValD (VarP n) (NormalB e) []]
            , BindS (VarP n') $ AppE (VarE 'newSTRef) $ LitE $ IntegerL 0
            ]
      putQ $ MkB (f . (<>) f') (back n' <> b) m'
      pure (n, n')

lift1 :: Name -> (ExpQ -> ExpQ -> ExpQ) -> D -> D
lift1 f f' xd = MkD $ do
  (x, x') <- runD xd
  y' <- newName "y'"
  deriv <- f' (varE x) (varE y')
  hashcons (AppE (VarE f) $ VarE x) $ \n' ->
    [ BindS (VarP y') $ AppE (VarE 'readSTRef) $ VarE n'
    , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE x') deriv
    ]

lift2 :: Name -> (ExpQ -> ExpQ -> ExpQ -> ExpQ) -> (ExpQ -> ExpQ -> ExpQ -> ExpQ) -> D -> D -> D
lift2 f f1' f2' xd yd = MkD $ do
  (x, x') <- runD xd
  (y, y') <- runD yd
  z' <- newName "z'"
  derivX <- f1' (varE x) (varE y) (varE z')
  derivY <- f2' (varE x) (varE y) (varE z')
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
  putQ $ MkB id [] empty
  (y, y') <- runD $ f $ MkD $ pure (x, x')
  Just (MkB forward backward _) <- getQ
  let new = BindS (VarP x') $ AppE (AppTypeE (VarE 'newSTRef) $ VarT a) $ LitE $ IntegerL 0
      write = NoBindS $ AppE (AppE (VarE 'writeSTRef) $ VarE y') $ LitE $ IntegerL 1
      ret = NoBindS $ UInfixE (TupE [Just $ VarE y, Nothing]) (VarE '(<$>)) $ AppE (VarE 'readSTRef) $ VarE x'
  pure $ LamE [SigP (VarP x) $ VarT a] $ DoE Nothing $ new : forward (write : backward <> [ret])
