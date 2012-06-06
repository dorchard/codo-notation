> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TypeOperators #-}

> import Language.Haskell.Codo
> import Control.Comonad.Alt
> import Data.Monoid

> import Control.Compose
> import Context

Use a "comonad transformer" to define !the dynamic programming comonad
as the composite of the InContext and product comonads.

> type DynP x = ((,) ([x], [x])) :. (InContext (Int, Int))

> instance ComonadDist ((,) x) (InContext s) where
>     cdist (x, InContext s c) = InContext (\c -> (x, s c)) c

> instance ComonadTransComp ((,) x) where
>     liftC_comp (O (x, a)) = a


> levenshtein :: DynP Char Int -> Int
> levenshtein = [codo| _ => -- Initialise first row and column
>                           d    <- levenshtein _
>                           dn   <- (coreturn d) + 1
>                           d0   <- (constant 0) `fbyXl` dn
>                           d'   <- d0 `fbyYl` dn
>                           -- Shift (-1, 0), (0, -1), (-1, -1)
>                           d_w  <- d !!! (-1, 0)
>                           d_n  <- d !!! (0, -1)
>                           d_nw <- d !!! (-1, -1)
>                           -- Body
>                           d'' <- if (correspondingX d == correspondingY d) then
>                                     coreturn d_nw
>                                  else minimum [(coreturn d_w) + 1,
>                                                (coreturn d_n) + 1,
>                                                (coreturn d_nw) + 1]
>                           d' `thenXYl` d''  |]

> edit_distance x y = levenshtein <<= (O ((' ':x, ' ':y), InContext undefined (0, 0)))

Operations on dynamic programming grids
 
> (!!!) :: DynP x a -> (Int, Int) -> a
> (!!!) = flip (\x -> (ixRelative x) . liftC_comp)

> -- Relative indexing of the grid - can be generalised
> ixRelative :: (Int, Int) -> InContext (Int, Int) a -> a
> ixRelative (x1, x2) (InContext s c@(c1, c2)) = s (c1 + x1, c2 + x2)

> correspondingX, correspondingY :: DynP x a -> x
> correspondingX (O ((x, y), (InContext s c@(c1, c2)))) = x!!c1
> correspondingY (O ((x, y), (InContext s c@(c1, c2)))) = y!!c2

> fbyXl x y = fbyX (liftC_comp x) (liftC_comp y)
> fbyYl x y = fbyY (liftC_comp x) (liftC_comp y)
> thenXYl x y = thenXY (liftC_comp x) (liftC_comp y)

> fbyX :: InContext (Int, Int) a -> InContext (Int, Int) a -> a
> fbyX (InContext s c@(c1, c2)) (InContext s' c'@(c1', c2')) = 
>            if (c1 == 0 && c1' == 0) then s (0, c2)
>            else s' (c1' - 1, c2')

> fbyY :: InContext (Int, Int) a -> InContext (Int, Int) a -> a
> fbyY (InContext s c@(c1, c2)) (InContext s' c'@(c1', c2')) = 
>            if (c2 == 0 && c2' == 0) then s (c1, 0)
>            else s' (c1', c2' - 1)


 fbyXY :: InContext (Int, Int) a -> InContext (Int, Int) a -> a
 fbyXY (InContext s c@(c1, c2)) (InContext s' c'@(c1', c2')) = 
                  if ((c1 == 0 || c2 == 0) && (c1' == 0 || c2' == 0)) then
                     s (max c1 c1', max c2 c2')
                  else
                      s' (c1' - 1, c2' - 1)n fst $ s c

> thenXY :: InContext (Int, Int) a -> InContext (Int, Int) a -> a
> thenXY (InContext s c@(c1, c2)) (InContext s' c'@(c1', c2')) = 
>                    if ((c1 == 0 && c1' == 0) || (c2 == 0 && c2' == 0)) then
>                         s (c1, c2)
>                    else s' (c1', c2')

> constant :: a -> DynP x a
> constant x = O (([], []), InContext (\c -> x) (0, 0))

Output functions

> output :: Show a => DynP Char a -> String
> output (O ((x, y), (InContext s c))) =
>         let top = "  " ++ foldr (\c -> \r -> [c] ++ " " ++ r) "" x ++ "\n"
>             row v = [y!!v] ++ (show $ map (\u -> s (u,v)) [0..(length x - 1)]) ++ "\n"
>         in top ++ concatMap row [0..(length y - 1)]




