> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}

> import Language.Haskell.Codo
> import Control.Comonad.Alt
> import Data.Monoid

> data DynProg x a = DynProg ((Int, Int) -> a) [x] [x] (Int, Int)

> replace x y [] = []
> replace x y (a:as) | x==[a]    = y ++ (replace x y as)
>                    | otherwise = [a] ++ (replace x y as)

> instance (Show a, Show x) => Show (DynProg x a) where
>     show (DynProg s x y c) =
>         let top = foldr (\c -> \r -> (show c) ++ " " ++ r) "" x ++ "\n"
>             row v = (show $ y!!v) ++ (show $ map (\u -> s (u,v)) [0..(length x - 1)]) ++ "\n"
>         in top ++ concatMap row [0..(length y - 1)]

> output :: Show a => DynProg Char a -> String
> output (DynProg s x y c) =
>         let top = "  " ++ foldr (\c -> \r -> [c] ++ " " ++ r) "" x ++ "\n"
>             row v = [y!!v] ++ (show $ map (\u -> s (u,v)) [0..(length x - 1)]) ++ "\n"
>         in top ++ concatMap row [0..(length y - 1)]

> instance Comonad (DynProg x) where
>     coreturn (DynProg s _ _ c) = s c

>     (DynProg s x y c) =>> f = 
>         DynProg (\c' -> f (DynProg s x y c')) x y c

> at :: DynProg x a -> (Int, Int) -> a
> at (DynProg s _ _ _) c = s c

> current :: DynProg x a -> (Int, Int)
> current (DynProg _ _ _ c) = c

> -- Relative indexing of the grid
> ixRelative :: (Int, Int) -> DynProg x a -> a
> ixRelative (x1, x2) (DynProg s _ _ c@(c1, c2)) = s (c1 + x1, c2 + x2)

> ixRelativeZ :: (Int, Int) -> DynProg x a -> a
> ixRelativeZ (x1, x2) (DynProg s _ _ c@(c1, c2)) = let y1 = c1 + x1
>                                                       y2 = c2 + x2
>                                                       y1' = if y1 < 0 then 0 else y1
>                                                       y2' = if y2 < 0 then 0 else y2
>                                                   in s (y1', y2')

> prevX, nextX, prevY, nextY, prevXY, nextXY :: DynProg x a -> a
> prevX = ixRelativeZ (-1, 0)
> prevY = ixRelativeZ (0, -1)
> prevXY = ixRelativeZ (-1, -1)
> nextX = ixRelative (1, 0)
> nextY = ixRelative (0, 1)
> nextXY = ixRelative (1, 1)

> correspondingX, correspondingY :: DynProg x a -> x
> correspondingX (DynProg s x y c@(c1, c2)) = x!!c1
> correspondingY (DynProg s x y c@(c1, c2)) = y!!c2

> fbyXu :: DynProg x a -> DynProg y a -> a
> fbyXu (DynProg s _ _ c@(c1, c2)) (DynProg s' _ _ c'@(c1', c2'))
>          = if (c1 == 0 && c1' == 0) then s (0, c2)
>            else s' (c1' - 1, c2')

> fbyYu :: DynProg x a -> DynProg y a -> a
> fbyYu (DynProg s _ _ c@(c1, c2)) (DynProg s' _ _ c'@(c1', c2'))
>          = if (c2 == 0 && c2' == 0) then s (c1, 0)
>            else s' (c1', c2' - 1)

> fbyXYu :: DynProg x a -> DynProg y a -> a
> fbyXYu (DynProg s _ _ c@(c1, c2)) (DynProg s' _ _ c'@(c1', c2'))
>                = if ((c1 == 0 || c2 == 0) && (c1' == 0 || c2' == 0)) then
>                     s (max c1 c1', max c2 c2')
>                  else
>                      s' (c1' - 1, c2' - 1)

> fbyX x y = fbyX' (czip (x, y))
> fbyX' :: DynProg x (a, a) -> a
> fbyX' (DynProg s _ _ c@(c1, c2)) = if c1==0 then fst $ s (0, c2)
>                                             else snd $ s (c1 - 1, c2)

> fbyY x y = fbyY' (czip (x, y))
> fbyY' :: DynProg x (a, a) -> a
> fbyY' (DynProg s _ _ c@(c1, c2)) = if c2==0 then fst $ s (c1, 0)
>                                             else snd $ s (c1, c2 - 1)

> fbyXY x y = fbyXY' (czip (x, y))
> fbyXY' :: DynProg x (a, a) -> a
> fbyXY' (DynProg s _ _ c@(c1, c2)) = if (c1 == 0 || c2 == 0) then fst $ s c
>                                       else snd $ s (c1 - 1, c2 - 1)

> -- pre condition: the dyn prog paramerters are equal
> instance ComonadZip (DynProg x) where
>     czip ((DynProg s l t c@(c1, c2)), (DynProg s' _ _ d@(d1, d2))) = 
>         let (y1, y2) = (max c1 d1, max c2 d2)
>         in   DynProg (\(x1, x2) -> let c' = (x1 - y1 + c1, x2 - y2 + c2)
>                                        d' = (x1 - y1 + d1, x2 - y2 + d2)
>                                    in (s c', s' d')) l t (y1, y2)

> instance ComonadFix (DynProg x) where
>     cfix f = let (DynProg s l t c) = coextend f (DynProg s l t c)
>              in DynProg s l t (0, 0)

> instance ComonadFixI (DynProg x) where
>     cfixi (DynProg _ l t c) f = let (DynProg s' _ _ _) = coextend f (DynProg s' l t c)
>                                 in DynProg s' l t c

> constant :: a -> DynProg x a
> constant x = DynProg (\c -> x) [] [] (0, 0)


> natX :: Num a => DynProg () a
> natX = cfixi (DynProg undefined [(), (), (), (), (), ()] [()] (0, 0))

>               [codo| n => n' <- (coreturn n) + 1
>                           (constant 0) `fbyX` n' |]

> levenshtein :: DynProg Char Int -> Int
> levenshtein = [codo| d => -- Initialise first row and column
>                           dn   <- (coreturn d) + 1
>                           d0   <- (constant 0) `fbyX` dn
>                           d'   <- d0 `fbyY` dn
>                           -- Shift (-1, 0), (0, -1), (-1, -1)
>                           d_w  <- prevX d
>                           d_n  <- prevY d
>                           d_nw <- prevXY d
>                           -- Body
>                           d'' <- if (correspondingX d == correspondingY d) then
>                                     coreturn d_nw
>                                   else minimum [(coreturn d_w) + 1,
>                                                (coreturn d_n) + 1,
>                                                (coreturn d_nw) + 1]
>                           d''' <- nextXY d''
>                           d' `fbyXY` d'''  |]

> dyn :: (DynProg Char a -> a) -> String -> String -> DynProg Char a
> dyn f x y = cfixi (DynProg undefined (' ':x) (' ':y) (0, 0)) f

e.g. putStr $ output $ dyn levenshtein "hello" "'elloa"


> (!!!) = flip ixRelativeZ

> levenshtein' :: DynProg Char Int -> Int
> levenshtein' = [codo| d => -- Initialise first row and column
>                            dn   <- (coreturn d) + 1
>                            d0   <- (constant 0) `fbyX` dn
>                            d'   <- d0 `fbyY` dn
>                            -- Shift (-1, 0), (0, -1), (-1, -1)
>                            d_w  <- d !!! (-1, 0)
>                            d_n  <- d !!! (0, -1)
>                            d_nw <- d !!! (-1, -1)
>                            -- Body
>                            d'' <- if (correspondingX d == correspondingY d) then
>                                      coreturn d_nw
>                                   else minimum [(coreturn d_w) + 1,
>                                                 (coreturn d_n) + 1,
>                                                 (coreturn d_nw) + 1]
>                            d''' <- nextXY d''
>                            d' `fbyXY` d'''  |]

> levenshtein'' :: DynProg Char Int -> Int
> levenshtein'' = [codo| _ => -- Initialise first row and column
>                             d    <<- levenshtein''
>                             dn   <- (coreturn d) + 1
>                             d0   <- (constant 0) `fbyX` dn
>                             d'   <- d0 `fbyY` dn
>                             -- Shift (-1, 0), (0, -1), (-1, -1)
>                             d_w  <- d !!! (-1, 0)
>                             d_n  <- d !!! (0, -1)
>                             d_nw <- d !!! (-1, -1)
>                             -- Body
>                             d'' <- if (correspondingX d == correspondingY d) then
>                                       coreturn d_nw
>                                    else minimum [(coreturn d_w) + 1,
>                                                  (coreturn d_n) + 1,
>                                                  (coreturn d_nw) + 1]
>                             d''' <- nextXY d''
>                             d' `fbyXY` d'''  |]

> thenXY :: DynProg x a -> DynProg x a -> a
> thenXY (DynProg s _ _ (c1, c2)) (DynProg t _ _ (c1', c2')) =
>                    if ((c1 == 0 && c1' == 0) || (c2 == 0 && c2' == 0)) then
>                         s (c1, c2)
>                    else t (c1', c2')

> levenshtein3 :: DynProg Char Int -> Int
> levenshtein3 = [codo| w => -- Initialise first row and column
>                            d    <- levenshtein3 w
>                            dn   <- (coreturn d) + 1
>                            d0   <- (constant 0) `fbyX` dn
>                            d'   <- d0 `fbyY` dn
>                            -- Shift (-1, 0), (0, -1), (-1, -1)
>                            d_w  <- d !!! (-1, 0)
>                            d_n  <- d !!! (0, -1)
>                            d_nw <- d !!! (-1, -1)
>                            -- Body
>                            d'' <- if (correspondingX d == correspondingY d) then
>                                      coreturn d_nw
>                                   else minimum [(coreturn d_w) + 1,
>                                                 (coreturn d_n) + 1,
>                                                 (coreturn d_nw) + 1]
>                            d' `thenXY` d''  |]


 levenshtein3 w = let d = w =>> levenshtein3
                      dn = 
                      

coextend levenshtein'' (DynProg undefined (' ':"hello") (' ':"hola" ) (0,0))
coextend levenshtein3 (DynProg undefined (' ':"hello") (' ':"hola" ) (0,0))

cfix essentially happens inside
