> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE QuasiQuotes #-}

> import Control.Comonad.Alt
> import Language.Haskell.Codo

> import Data.Array

> data PArray i a = PA (Array i a) i deriving Show

> instance Ix i => Comonad (PArray i) where
>    current (PA arr c) = arr!c
>    f <<= (PA x c)  =
>      let  es' = map (\i -> (i, f (PA x i))) (indices x)
>      in   PA (array (bounds x) es') c

> laplace1D :: Fractional a => PArray Int a -> a
> laplace1D (PA a i) =
>  let  (b1, b2) = bounds a
>  in  if (i>b1 && i<b2)
>      then  a!(i-1) - 2*(a!i) + a!(i+1)
>      else  0.0


> data PBArray i a = PBA (Array i a) i (i, i) deriving Show

> instance Ix i => Comonad (PBArray i) where
>    current (PBA arr c _) = arr!c
>    f <<= (PBA x c b) =
>      let  es' = map (\i -> (i, f (PBA x i b))) (indices x)
>      in   PBA (array (bounds x) es') c b


> withInterior :: Ord i => (PBArray i a -> b) -> (PBArray i a -> b) -> (PBArray i a -> b)
> withInterior f g x@(PBA a c (b1, b2)) = if (c>=b1 && c<b2)
>                                      then g x
>                                      else f x

> withExterior :: Ord i => (PBArray i a -> b) -> (PBArray i a -> b) -> (PBArray i a -> b)
> withExterior f g x@(PBA a c (b1, b2)) = if (c>=b1 && c<b2)
>                                      then f x
>                                      else g x

> laplace1Db :: Fractional a => PBArray Int a -> a
> laplace1Db = (\(PBA a i _) -> a!(i-1) - 2*(a!i) + a!(i+1))
>                  `withExterior` current

> localMean1Db :: Fractional a => PBArray Int a -> a
> localMean1Db = (\(PBA a i _) -> (a!(i-1) + a!i + a!(i+1)) / 3.0)
>                   `withExterior` current

> filterC :: Comonad c => (a -> Bool) -> a -> c a -> a
> filterC p x a = if p (current a) then x else current a


Example array values

> x = PBA (array (0,4) [(0, 0.0), (1, 0.5), (2, 0.7), (3, 0.5), (4, 0.0)]) (0::Int) (1,4)
> y = PBA (array (0,4) [(0, 0.0), (1, 0.5), (2, 0.7), (3, 0.0), (4, 1.0)]) (0::Int) (1,4)


> -- Examples in the paper

> prog1 = [codo| x => y <- laplace1Db x
>                     z <- localMean1Db y
>                     current z |]

> plus :: (Comonad c, Num a) => c a -> c a -> a
> plus x y = current x + current y

> prog2 = [codo| a => b <- localMean1Db a
>                     c <- laplace1Db b
>                     d <- plus b c
>                     localMean1Db d |]

> -- equivalent translation
> prog2e a =  let b = localMean1Db <<= a
>                 d = (\b' -> let c = laplace1Db <<= b'
>                             in plus b' c) <<= b
>             in localMean1Db d

> -- non-pointwise (bad) version (by hand)
> prog2bad a = let b = localMean1Db <<= a
>                  c = laplace1Db <<= b
>                  d = (plus b) <<= c
>              in localMean1Db d

> -- non-pointwise with codo
> prog2' = [codo| a => b <- localMean1Db a
>                      (codo  b' => c <- laplace1Db b'
>                                   d <- plus b c
>                                   localMean1Db d) b  |]

> prog3 = [codo| (x, y) => a <- laplace1Db x
>                          b <- laplace1Db y
>                          (current a) + (current b) |]

prog3 <<= (czip (x, y))

> instance (Eq i, Ix i) => ComonadZip (PBArray i) where
>     czip (PBA a c (b1, b2), PBA a' c' (b1', b2')) = 
>         if (c/=c' || b1 /= b1' || b2 /= b2') then 
>             error "Cursor and boundaries must be the same for zipping"
>         else let es'' = map (\i -> (i, (a!i, a'!i))) (indices a)
>              in PBA (array (bounds a) es'') c (b1, b2)


--------------------
Other expermintation with abstractions on boundary testing

                                 
> laplace1Dc, localMean1Dc :: Floating a => PBArray Int a -> a
> laplace1Dc (PBA a i _) = a!(i-1) - 2*(a!i) + a!(i+1)
> localMean1Dc (PBA a i _) = (a!(i-1) + a!i + a!(i+1)) / 3.0

> withBoundary :: (Num i, Ix i) => PBArray i a -> PBArray i a -> a
> withBoundary x y = withBoundary' . czip $ (x, y)

> withBoundary' :: (Num i, Ix i) => PBArray i (a, a) -> a
> withBoundary' (PBA a c (b1, b2)) = if (c>=b1 && c<b2) then
>                                        fst $ a!c
>                                    else
>                                        snd $ a!c

> withExterior' :: (Num i, Ix i) => PBArray i a -> PBArray i a -> a -- assumes synchronisation
> withExterior' (PBA a c (b1, b2)) (PBA a' _ _) = if (c>=b1 && c<b2) then a!c
>                                                                    else a'!c

> foo3 = [codo|  a => b <- laplace1Dc a
>                     b' <- b `withBoundary` a
>                     c <- localMean1Dc b'
>                     c' <- c `withBoundary` a
>                     d <- (current b') `min` (current c')
>                     filterC (<0.3) 0.3 d |]

> boo4 = [codo|  a => b <- laplace1Dc a
>                     b' <- b `withBoundary` a
>                     c <- localMean1Dc b'
>                     c' <- c `withBoundary` a
>                     d <- (current b') `min` (current c')
>                     w <- localMean1Dc d
>                     w `withBoundary` a |]

> boo4' = [codo|  a => b <- laplace1Dc a
>                      b' <- b `withExterior'` a
>                      c <- localMean1Dc b'
>                      c' <- c `withExterior'` a
>                      d <- (current b') `min` (current c')
>                      w <- localMean1Dc d
>                      w `withExterior'` a|]

> boo4'' = [codo|  a => b <- laplace1Dc `withExterior` current $ a
>                       c <- localMean1Dc `withExterior` current $ b
>                       d <- (current b) `min` (current c)
>                       localMean1Dc `withExterior` current $ d |]



------------


> xa = PA (array (0,4) [(0, 3.0), (1, 0.5), (2, 0.7), (3, 0.5), (4, 0.0)]) (0::Int)

> prog1a = [codo| x => y <- laplace1D x
>                      laplace1D y |]

> prog2a = [codo| x => y <- laplace1D x
>                      z <- (current x) + (current y)
>                      current z |]

> prog3a = [codo| x => y <- laplace1D x
>                      z <- (current x) + (current y)
>                      laplace1D z |]

