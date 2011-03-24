> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> -- KNOWN TO WORK IN GHC-6.12 

> import Data.Array.IArray

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

> data Arr i e = Arr (Array i e) i

> instance (Show a, Show i, Ix i) => Show (Arr i a) where
>     show (Arr arr i) = let es = elems arr
>                        in (show es)++"@"++(show i)

> instance Ix i => Comonad (Arr i) where
>   coreturn (Arr arr i) = arr!i
>   (Arr arr i) =>> f = let  (b1, b2) = bounds arr
>                            es = map (\c -> (c, f (Arr arr c))) (range (b1, b2))
>                            arr' = array (b1, b2) es
>                       in
>                            Arr arr' i

> class Dist c m where
>    dist :: c (m a) -> m (c a)

> instance (Monad m, Ix i) => Dist (Arr i) m where
>    dist (Arr arr i) = do let (b1, b2) = bounds arr
>                          let f = (\c' -> coreturn (Arr arr c'))
>                          vals <- mapM f (range (b1, b2))
>                          return $ Arr (listArray (b1, b2) vals) i

> access :: Ix i => Arr i a -> i -> Maybe a
> access (Arr arr _) i = let (b1, b2) = bounds arr
>                        in if (i<b1 || i>=b2) then
>                               Nothing
>                           else
>                               Just (arr!i)

> cursor :: Arr i a -> i
> cursor (Arr arr i) = i

> laplace1D_fail :: Arr i a -> Maybe a
> laplace1D_fail = [$bido|(a) c <- access a (cursor a)
>                             l <- access a (cursor a - 1)
>                             r <- access a (cursor a + 1)
>                             return $ (coreturn l) + (coreturn r) - 2*(coreturn c)|]

> arrA :: Arr Int Int
> arrA = Arr (array (0, 10) (map (\i -> (i, i*2)) [0..10])) 0

> arrA_fail = dist (cobind laplace1D_fail (Just arrA))
