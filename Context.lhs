> module Context where

> import Data.Monoid
> import Control.Comonad.Alt

InContext or CoState comonad - models context-aware computations

> data InContext c a = InContext (c -> a) c 

> instance Comonad (InContext c) where
>     current (InContext s c) = s c
>     k <<= (InContext s c) = InContext (\c' -> k (InContext s c')) c

> at :: InContext c a -> c -> a
> at (InContext s _) c' = s c'

> context :: InContext c a -> c
> context (InContext s c) = c


Param or exponent comonad - models context-oblivious computations

> data Param x a = Param (x -> a)

> instance Monoid x => Comonad (Param x) where
>     current (Param f) = f mempty
>     k <<= (Param f) = Param (\x' -> k (Param (\x -> f (mappend x x'))))