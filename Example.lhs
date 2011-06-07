> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> -- KNOWN TO WORK IN GHC-6.12 

> import Language.Haskell.SyntacticSugar
> import Prelude hiding (odd, const)

> -- Comonads class

> class Comonad c where
>     coreturn :: c a -> a
>     (=>>) :: c a -> (c a -> b) -> c b

> cobind :: Comonad c => (c a -> b) -> c a -> c b
> cobind = flip (=>>)

> cmap :: Comonad c => (a -> b) -> c a -> c b
> cmap f = cobind (f . coreturn)

> data Stream a = Stream (Int -> a) Int

> unStream (Stream s c) = s

> instance Show a => Show (Stream a) where
>     show (Stream s c) = (show (map s [0..20]))++("@"++(show c))

> instance Comonad Stream where
>     coreturn (Stream s c) = s c
>     ~(Stream s c) =>> f = Stream (\c' -> f (Stream s c')) c

> next :: Stream a -> a
> next ~(Stream s c) = s (c+1)

> fby :: Stream a -> Stream a -> a
> fby ~(Stream s c) ~(Stream t d) = if (c==0 && d==0) then s 0 else t (d-1)

> plus :: (Num a, Comonad c) => c a -> c a -> a
> plus x y = (coreturn x) + (coreturn y)

> const :: a -> Stream a
> const x = Stream (\_ -> x) 0

> class Comonad c => CFix c where
>     cfix :: (c a -> a) -> c a

> instance CFix Stream where
>     cfix f = let (Stream s _) = cobind f (Stream s 0)
>              in (Stream s 0)

> n :: Num a => Stream a
> n = cfix [$codo|(n) y <- plus n (const 1)
>                     fby (const 0) y |]

> fib :: Num a => Stream a
> fib = cfix [$codo|(fib) fibn2 <- (next fib) + (coreturn fib)
>                         fibn1 <- fby (const 1) fibn2
>                         fibn0 <- fby (const 0) fibn1
>                         coreturn fibn0 |]

The following shows the use of lets inside codo

> fib2 :: Num a => Stream a
> fib2 = cfix [$codo|(fib) let one = const 1
>                          let zero = const 0
>                          fibn2 <- (next fib) + (coreturn fib)
>                          fibn1 <- fby one fibn2
>                          fibn0 <- fby zero fibn1
>                          coreturn fibn0 |]

The following shows the use of implicit parameter shadowing
(I may remove this feature, but sometimes it seems useful)

> fib3 :: Num a => Stream a
> fib3 = cfix [$codo|(fib) (next fib) + (coreturn fib)
>                          fby (const 1) fib
>                          fby (const 0) fib |]

