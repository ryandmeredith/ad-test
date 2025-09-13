{-# LANGUAGE TemplateHaskellQuotes #-}

module AD (D, autodiff) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Cont (ContT (ContT, runContT))
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Data.Map (Map, empty, insert, lookup)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Language.Haskell.TH (
  Exp (AppE, ConE, DoE, InfixE, LamE, LitE, TupE, UInfixE, VarE),
  ExpQ,
  Lit (IntegerL),
  Name,
  Pat (TupP, VarP),
  Q,
  Stmt (BindS, NoBindS),
  mkName,
  newName,
 )
import Prelude hiding (lookup)

data DAG = MkB ([Stmt] -> [Stmt]) (Map Exp (Name, Name))

newtype D = MkD (StateT DAG Q (Name, Name))

hashcons :: Exp -> StateT DAG Q (Name, Name)
hashcons e = do
  MkB s m <- get
  case lookup e m of
    Just n -> pure n
    Nothing -> do
      n <- lift $ newName "x"
      n' <- lift $ newName "x'"
      let s' = s . (BindS (TupP [VarP n, VarP n']) e :)
          m' = insert e (n, n') m
      put $ MkB s' m'
      pure (n, n')

lift0 :: Exp -> StateT DAG Q (Name, Name)
lift0 e =
  hashcons $
    UInfixE (TupE [Just e, Nothing]) (VarE '(<$>)) $
      AppE (VarE 'lift) $
        AppE (VarE 'newSTRef) $
          LitE $
            IntegerL 0

lift1 :: Exp -> (Exp -> Exp -> Exp) -> D -> D
lift1 f f' (MkD xs) = MkD $ do
  (x, x') <- xs
  let k = mkName "k"
      r = mkName "r"
      y = mkName "y"
      y' = mkName "y'"
  hashcons $
    AppE (ConE 'ContT) $
      LamE [VarP k] $
        DoE
          Nothing
          [ BindS (VarP r) $ AppE (VarE 'newSTRef) $ LitE $ IntegerL 0
          , BindS (VarP y) $ AppE (VarE k) $ TupE [Just $ AppE f $ VarE x, Just $ VarE r]
          , BindS (VarP y') $ AppE (VarE 'readSTRef) $ VarE r
          , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE x') $ f' (VarE x) (VarE y')
          , NoBindS $ AppE (VarE 'pure) $ VarE y
          ]

lift2 :: Exp -> (Exp -> Exp -> Exp -> Exp) -> (Exp -> Exp -> Exp -> Exp) -> D -> D -> D
lift2 f f1' f2' (MkD xs) (MkD ys) = MkD $ do
  (x, x') <- xs
  (y, y') <- ys
  let k = mkName "k"
      r = mkName "r"
      z = mkName "z"
      z' = mkName "z'"
  hashcons $
    AppE (ConE 'ContT) $
      LamE [VarP k] $
        DoE
          Nothing
          [ BindS (VarP r) $ AppE (VarE 'newSTRef) $ LitE $ IntegerL 0
          , BindS (VarP z) $ AppE (VarE k) $ TupE [Just $ UInfixE (VarE x) f (VarE y), Just $ VarE r]
          , BindS (VarP z') $ AppE (VarE 'readSTRef) $ VarE r
          , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE x') $ f1' (VarE x) (VarE y) (VarE z')
          , NoBindS $ AppE (AppE (VarE 'modifySTRef') $ VarE y') $ f2' (VarE x) (VarE y) (VarE z')
          , NoBindS $ AppE (VarE 'pure) $ VarE z
          ]

instance Num D where
  (+) =
    lift2
      (VarE '(+))
      (\_ _ z' -> InfixE Nothing (VarE '(+)) $ Just z')
      (\_ _ z' -> InfixE Nothing (VarE '(+)) $ Just z')
  (-) =
    lift2
      (VarE '(-))
      (\_ _ z' -> InfixE Nothing (VarE '(+)) $ Just z')
      (\_ _ z' -> InfixE Nothing (VarE '(-)) $ Just z')
  (*) =
    lift2
      (VarE '(*))
      (\_ y z' -> InfixE Nothing (VarE '(+)) $ Just $ UInfixE z' (VarE '(*)) y)
      (\x _ z' -> InfixE Nothing (VarE '(+)) $ Just $ UInfixE z' (VarE '(*)) x)
  negate = lift1 (VarE 'negate) $ \_ y' -> InfixE Nothing (VarE '(-)) $ Just y'
  abs = lift1 (VarE 'abs) $ \x y' ->
    InfixE Nothing (VarE '(+)) $ Just $ UInfixE y' (VarE '(*)) $ AppE (VarE 'signum) x
  signum (MkD xs) = MkD $ xs >>= lift0 . AppE (VarE 'signum) . VarE . fst
  fromInteger = MkD . lift0 . LitE . IntegerL

autodiff :: (D -> D) -> ExpQ
autodiff f = do
  x <- newName "x"
  x' <- newName "x'"
  z <- newName "z"
  z' <- newName "z'"
  r <- newName "r"
  r' <- newName "r'"
  let MkD m = f $ MkD $ pure (x, x')
  ((y, y'), MkB s _) <- runStateT m $ MkB id empty
  pure $
    LamE [VarP x] $
      DoE
        Nothing
        [ BindS (VarP x') $ AppE (VarE 'newSTRef) $ LitE $ IntegerL 0
        , BindS (VarP r)
            $ AppE
              ( AppE (VarE 'runContT) $
                  DoE Nothing $
                    s [NoBindS $ AppE (VarE 'pure) $ TupE [Just (VarE y), Just (VarE y')]]
              )
            $ LamE [TupP [VarP z, VarP z']]
            $ UInfixE (VarE z) (VarE '(<$))
            $ AppE (AppE (VarE 'writeSTRef) $ VarE z')
            $ LitE
            $ IntegerL 1
        , BindS (VarP r') $ AppE (VarE 'readSTRef) $ VarE x'
        , NoBindS $ AppE (VarE 'pure) $ TupE [Just (VarE r), Just (VarE r')]
        ]
