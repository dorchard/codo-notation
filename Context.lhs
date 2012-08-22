> module Context where

> import Data.Monoid
> import Control.Comonad

InContext or CoState comonad - models context-aware computations

> data InContext c a = InContext (c -> a) c 

> instance Comonad (InContext c) where
>     extract (InContext s c) = s c
>     extend k (InContext s c) = InContext (\c' -> k (InContext s c')) c

> instance Functor (InContext c) where
>     fmap f = extend (f . extract)


> at :: InContext c a -> c -> a
> at (InContext s _) c' = s c'

> context :: InContext c a -> c
> context (InContext s c) = c


Param or exponent comonad - models context-oblivious computations

 instance Monoid x => Comonad ((->) x) where
     extract f = f mempty
     extend k f = (\x' -> k (\x -> f (mappend x x')))

 instance Monoid x => Functor ((->) x) where
     fmap f = extend (f . extract)

Product comonad

 instance Comonad ((,) x) where
     extract (x, a) = a
     extend f (x, a) = (x, f (x, a))

 instance Functor ((,) x) where
     fmap f = extend (f . extract)
