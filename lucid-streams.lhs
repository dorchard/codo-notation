> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeSynonymInstances #-}

> {-# LANGUAGE NoMonomorphismRestriction #-}

> import Control.Comonad
> import Language.Haskell.Codo

> import Context

> type Stream a = InContext Int a

> -- n = 0 fby n + 1
> nat' :: Num a => Stream b -> a
> nat' = [codo| _ => n' <- (nat' _) + 1
>                    (constant 0) `fby` n' |]

> nat = nat' <<= (constant ())

> -- fib = 0 fby 1 fby (fib + next fib)
> fib' :: Num a => Stream () -> a
> fib' = [codo| _ => fib <- fib' _
>                    fibn2 <- (next fib) + (extract fib)
>                    fibn1 <- (constant 1) `fby` fibn2
>                    (constant 0) `fby` fibn1 |]

> fib = fib' <<= (constant ())


> -- Example of nested tuple patterns
> tup3 = [codo| (x, (y, z)) => a <- (extract y) + (extract z)
>                              x `fby` a |] 

Stream operations

> next :: Stream a -> a
> next ~(InContext s c) = s (c+1)

> fby :: Stream a -> Stream a -> a
> fby ~(InContext s c) ~(InContext t d) = if (c==0 && d==0) then s 0 else t (d-1)

> constant :: a -> Stream a
> constant x = InContext (\_ -> x) 0

Output function

> instance Show a => Show (Stream a) where
>     show (InContext s c) = (show (map s [0..20]))++("@"++(show c))
