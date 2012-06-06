> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> -- Alternative class to Kmett's Control.Comonad

> module Control.Comonad.Alt where

> import Control.Compose

Minimal implementations:
        - coreturn, cmap & cojoin
        - coreturn, =>>

> class Comonad c where
>     current :: c a -> a

>     (<<=) :: Comonad c => (c a -> b) -> c a -> c b
>     f <<= x = (cmap f . cojoin) $ x

>     cojoin :: c a -> c (c a)
>     cojoin x = id <<= x

>     cmap :: (a -> b) -> c a -> c b
>     cmap f x = (f . current) <<= x

> (=>>) :: Comonad c => c a -> (c a -> b) -> c b
> (=>>) = flip (<<=)

> cobind :: Comonad c => (c a -> b) -> c a -> c b
> cobind = (<<=)

> coreturn :: Comonad c => c a -> a
> coreturn = current

> -- Magma comonad operation 
> -- (maybe be a (semi)-monoidal operation, but not necessarily)
> class Comonad c => ComonadZip c where
>     czip ::  (c a, c b) -> c (a, b)

> cmap2 :: (ComonadZip c, Comonad c) => (x -> y -> z) -> c x -> c y -> c z
> cmap2 f cx cy = cmap (uncurry f) (czip (cx, cy))


> -- Can be useful for programming
> class Comonad c => ComonadFix c where
>     cfix :: (c a -> a) -> c a
>     cfix f = cobind f (cfix f)

> -- Distributive law between comonads
> class ComonadDist c d where
>     cdist :: c (d a) -> d (c a)

> -- The composite of any two comonads with a (coherence preserving) distributive law
> -- forms a comonad
> instance (Comonad c, Comonad d, ComonadDist c d) => Comonad (c :. d) where
>     current (O x) = current . current $ x
>     cojoin (O x) = O . (cmap (cmap O)) . (cmap cdist) . (cmap (cmap cojoin)) . cojoin $ x
>     cmap f (O x) = O (cmap (cmap f) x)

> -- Comonad transformers
> class ComonadTrans t where
>     liftC :: Comonad c => t c a -> c a

> -- Comonad transformer for composites
> class ComonadTransComp t where
>     liftC_comp :: Comonad c => (t :. c) a -> c a

