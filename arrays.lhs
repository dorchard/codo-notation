> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE QuasiQuotes #-}

> {-# LANGUAGE NoMonomorphismRestriction #-}

> import Control.Comonad
> import Language.Haskell.Codo

> import qualified Control.Category as Math


> import Data.Array

> class Comonad c => ComonadZip c where
>     czip ::  (c a, c b) -> c (a, b)


> data PArray i a = PA (Array i a) i deriving Show

> instance Ix i => Comonad (PArray i) where
>    extract (PA arr c) = arr!c
>    extend f (PA x c)  =
>      let  es' = map (\i -> (i, f (PA x i))) (indices x)
>      in   PA (array (bounds x) es') c

> instance Ix i => Functor (PArray i) where
>     fmap f = extend (f . extract)

> laplace1D :: Fractional a => PArray Int a -> a
> laplace1D (PA a i) =
>  let  (b1, b2) = bounds a
>  in  if (i>b1 && i<b2)
>      then  a!(i-1) - 2*(a!i) + a!(i+1)
>      else  0.0


> data PBArray i a = PBA (Array i a) i (i, i) deriving Show

> instance Ix i => Comonad (PBArray i) where
>    extract (PBA arr c _) = arr!c
>    extend f (PBA x c b) =
>      let  es' = map (\i -> (i, f (PBA x i b))) (indices x)
>      in   PBA (array (bounds x) es') c b

> instance Ix i => Functor (PBArray i) where
>     fmap f = extend (f . extract)


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
>                  `withExterior` extract

> localMean1Db :: Fractional a => PBArray Int a -> a
> localMean1Db = (\(PBA a i _) -> (a!(i-1) + a!i + a!(i+1)) / 3.0)
>                   `withExterior` extract

> filterC :: Comonad c => (a -> Bool) -> a -> c a -> a
> filterC p x a = if p (extract a) then x else extract a


Example array values

> x = PBA (array (0,4) [(0, 0.0), (1, 0.5), (2, 0.7), (3, 0.5), (4, 0.0)]) (0::Int) (1,4)
> y = PBA (array (0,4) [(0, 0.0), (1, 0.5), (2, 0.7), (3, 0.0), (4, 1.0)]) (0::Int) (1,4)


> -- Examples in the paper

> prog1 = [codo| x => y <- laplace1Db x
>                     z <- localMean1Db y
>                     extract z |]

> plus :: (Comonad c, Num a) => c a -> c a -> a
> plus x y = extract x + extract y

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

> -- non-pointwise with context
> prog2' = [codo| a => b <- localMean1Db a
>                      (codo  b' => c <- laplace1Db b'
>                                   d <- plus b c
>                                   localMean1Db d) b  |]

> prog3 = [context| (x, y) => a <- laplace1Db x
>                             b <- laplace1Db y
>                             (extract a) + (extract b) |]

prog3 <<= (czip (x, y))

> instance (Eq i, Ix i) => ComonadZip (PBArray i) where
>     czip (PBA a c (b1, b2), PBA a' c' (b1', b2')) = 
>         if (c/=c' || b1 /= b1' || b2 /= b2') then 
>             error "Cursor and boundaries must be the same for zipping"
>         else let es'' = map (\i -> (i, (a!i, a'!i))) (indices a)
>              in PBA (array (bounds a) es'') c (b1, b2)

> instance (Eq i, Ix i) => ComonadZip (PArray i) where
>     czip (PA a c, PA a' c') = 
>         if (c/=c') then 
>             error "Cursor and boundaries must be the same for zipping"
>         else let es'' = map (\i -> (i, (a!i, a'!i))) (indices a)
>              in PA (array (bounds a) es'') c


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

> foo3 = [context|  a => b <- laplace1Dc a
>                        b' <- b `withBoundary` a
>                        c <- localMean1Dc b'
>                        c' <- c `withBoundary` a
>                        d <- (extract b') `min` (extract c')
>                        filterC (<0.3) 0.3 d |]

> boo4 = [context|  a => b <- laplace1Dc a
>                        b' <- b `withBoundary` a
>                        c <- localMean1Dc b'
>                        c' <- c `withBoundary` a
>                        d <- (extract b') `min` (extract c')
>                        w <- localMean1Dc d
>                        w `withBoundary` a |]

> boo4' = [context|  a => b <- laplace1Dc a
>                         b' <- b `withExterior'` a
>                         c <- localMean1Dc b'
>                         c' <- c `withExterior'` a
>                         d <- (extract b') `min` (extract c')
>                         w <- localMean1Dc d
>                         w `withExterior'` a|]

> boo4'' = [context|  a => b <- laplace1Dc `withExterior` extract $ a
>                          c <- localMean1Dc `withExterior` extract $ b
>                          d <- (extract b) `min` (extract c)
>                          localMean1Dc `withExterior` extract $ d |]



------------


> xa = PA (array (0,4) [(0, 3.0), (1, 0.5), (2, 0.7), (3, 0.5), (4, 0.0)]) (0::Int)
> xb = PA (array (0,4) [(0, 5.4), (1, 1.5), (2, 3.4), (3, 4.5), (4, 4.0)]) (0::Int)

> prog1a = [context| x => y <- laplace1D x
>                         laplace1D y |]

> prog2a = [context| x => y <- laplace1D x
>                         z <- (extract x) + (extract y)
>                         extract z |]

> prog3a = [context| x => y <- laplace1D x
>                         z <- (extract x) + (extract y)
>                         laplace1D z |]

---------------------

> -- ==================== 2D arrays ==============

> -- To simplify code, make tuples of numbers a number type themselves
> instance (Num a, Num b) => Num (a, b) where
>     (x, y) + (a, b) = (x + a, y  + b)
>     (x, y) - (a, b) = (x - a, y - b)
>     (x, y) * (a, b) = (x * a, y * b)
>     abs (x, y) = (abs x, abs y)
>     signum (x, y) = (signum x, signum y)
>     fromInteger x = (fromInteger x, fromInteger x)


> laplace2D, gauss2D :: Fractional a => PArray (Int, Int) a -> a
> laplace2D a = a ? (-1, 0) + a ? (1, 0) + a ? (0, -1) + a ? (0, 1) - 4 * a ? (0, 0)
> gauss2D a = (a ? (-1, 0) + a ? (1, 0) + a ? (0, -1) + a ? (0, 1) + 2 * a ? (0, 0)) / 6.0

> (?) :: (Ix i, Num a, Num i) => PArray i a -> i -> a
> (PA a i) ? i' = if (inRange (bounds a) (i+i')) then a!(i+i') else 0

> xx :: PArray (Int, Int) Double
> xx = PA (array ((0,0), (2,2)) [((0, 0), 1), ((0, 1), 1), ((0, 2), 2),
>                                ((1, 0), 1), ((1, 1), 2), ((1, 2), 3),
>                                ((2, 0), 3), ((2, 1), 2), ((2, 2), 1)]) (0,0)

> proj (PA arr _) = arr

> getData (PA arr _) = assocs arr

> contours :: PArray (Int, Int) Double -> Double
> contours = [context| x => y  <- gauss2D x
>                           z  <- gauss2D y
>                           w  <- (extract y) - (extract z)
>                           laplace2D w |]
  
> (^.) g f = g . extend f

> minus x y = extract x - extract y

> contours' = laplace2D
>           ^. (\y' -> minus y' ^. gauss2D $ y')
>           ^. gauss2D

 contours_bad' = laplace2D
                 ^. (minus 
                 ^. gauss2D
                 ^. gauss2D

> foo1 = [context| (a, b) => minus a b |]

> foo2 = [context| (a, b) => (a', b') <- extract $ czip (a, b)
>                            minus a' b' |]

