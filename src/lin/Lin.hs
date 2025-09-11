module Lin (Lin, app, zero, plus, minus, neg, scale) where

import Control.Category

newtype Lin a b = MkL (a -> b) deriving Category

{- HLINT ignore app "Eta reduce" -}
{-# NOINLINE app #-}
app :: Lin a b -> a -> b
app (MkL f) x = f x

{-# RULES
"linear/plus" forall (f :: Lin a a) x y. app f x + app f y = app f (x + y)
"linear/minus" forall (f :: Lin a a) x y. app f x - app f y = app f (x - y)
  #-}

zero :: Num b => Lin a b
zero = MkL $ const 0

plus :: Num b => Lin a b -> Lin a b -> Lin a b
plus f g = MkL $ \x -> app f x + app g x

minus :: Num b => Lin a b -> Lin a b -> Lin a b
minus f g = MkL $ \x -> app f x - app g x

neg :: Num a => Lin a b -> Lin a b
neg f = MkL $ \x -> app f $ negate x

scale :: Num a => a -> Lin a b -> Lin a b
scale x f = MkL $ \y -> app f (x * y)
