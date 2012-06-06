> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}

> import Language.Haskell.Codo
> import Control.Comonad.Alt

> import Context

2D dynamic programming example
Comonad is a composite of InContext and product comonad

> data DynP x a = DynP (InContext (Int, Int) a) [x] [x] 

Comonad definition

> instance Comonad (DynP x) where
>     current (DynP d _ _) = current d

>     f <<= (DynP (InContext s c) x y) = 
>         DynP (InContext (\c' -> f (DynP (InContext s c') x y)) c) x y

Levenshtein edit-distance example

> levenshtein :: DynP Char a -> Int
> levenshtein = [codo| _ => -- Initialise first row and column
>                           d    <- levenshtein _
>                           dn   <- (coreturn d) + 1
>                           d0   <- (constant 0) `fbyX` dn
>                           d'   <- d0 `fbyY` dn
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
>                           d' `thenXY` d''  |]

> edit_distance x y = levenshtein <<= (DynP (InContext undefined (0, 0)) (' ':x) (' ':y))

e.g.

*Main> edit_distance "hello" "hey"
' ' 'h' 'e' 'l' 'l' 'o' 
' '[0,1,2,3,4,5]
'h'[1,0,1,2,3,4]
'e'[2,1,0,1,2,3]
'y'[3,2,1,1,2,3]


Operations on dynamic programming grids

> (!!!) = flip ixRelative

> -- Relative indexing of the grid
> ixRelative :: (Int, Int) -> DynP x a -> a
> ixRelative (x1, x2) (DynP (InContext s c@(c1, c2)) _ _) = s (c1 + x1, c2 + x2)

> correspondingX, correspondingY :: DynP x a -> x
> correspondingX (DynP (InContext s c@(c1, c2)) x y) = x!!c1
> correspondingY (DynP (InContext s c@(c1, c2)) x y) = y!!c2

> fbyX :: DynP x a -> DynP y a -> a
> fbyX (DynP (InContext s c@(c1, c2)) x y) (DynP (InContext s' c'@(c1', c2')) _ _) 
>          = if (c1 == 0 && c1' == 0) then s (0, c2)
>            else s' (c1' - 1, c2')

> fbyY :: DynP x a -> DynP y a -> a
> fbyY (DynP (InContext s c@(c1, c2)) x y) (DynP (InContext s' c'@(c1', c2')) _ _) 
>          = if (c2 == 0 && c2' == 0) then s (c1, 0)
>            else s' (c1', c2' - 1)

 fbyXY :: DynP x a -> DynP y a -> a
 fbyXY (DynP (InContext s c@(c1, c2)) x y) (DynP (InContext s' c'@(c1', c2')) _ _) 
                = if ((c1 == 0 || c2 == 0) && (c1' == 0 || c2' == 0)) then
                     s (max c1 c1', max c2 c2')
                  else
                      s' (c1' - 1, c2' - 1)

> thenXY :: DynP x a -> DynP x a -> a
> thenXY (DynP (InContext s c@(c1, c2)) x y) (DynP (InContext s' c'@(c1', c2')) _ _) = 
>                    if ((c1 == 0 && c1' == 0) || (c2 == 0 && c2' == 0)) then
>                         s (c1, c2)
>                    else s' (c1', c2')

> constant :: a -> DynP x a
> constant x = DynP (InContext (\c -> x) (0, 0)) [] []

> -- Not used in this example
> prevX, nextX, prevY, nextY, prevXY, nextXY :: DynP x a -> a
> prevX = ixRelative (-1, 0)
> prevY = ixRelative (0, -1)
> prevXY = ixRelative (-1, -1)
> nextX = ixRelative (1, 0)
> nextY = ixRelative (0, 1)
> nextXY = ixRelative (1, 1)


Output functions

> instance (Show a, Show x) => Show (DynP x a) where
>     show (DynP (InContext s c) x y) =
>         let top = foldr (\c -> \r -> (show c) ++ " " ++ r) "" x ++ "\n"
>             row v = (show $ y!!v) ++ (show $ map (\u -> s (u,v)) [0..(length x - 1)]) ++ "\n"
>         in top ++ concatMap row [0..(length y - 1)]

 output :: Show a => DynP Char a -> String
 output (DynP (InContext s c) x y) =
         let top = "  " ++ foldr (\c -> \r -> [c] ++ " " ++ r) "" x ++ "\n"
             row v = [y!!v] ++ (show $ map (\u -> s (u,v)) [0..(length x - 1)]) ++ "\n"
         in top ++ concatMap row [0..(length y - 1)]


