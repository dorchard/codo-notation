> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}

> import Language.Haskell.Codo
> import Control.Comonad
> import Data.Monoid

> import Data.Array.IArray

Usea an array to do dynamic programming as opposed to the (inefficient) InContext

> data DynP x a = DynP (Array (Int, Int) a) [x] [x] (Int, Int) ((Int, Int), (Int, Int))

> instance Comonad (DynP x) where
>     extract (DynP a _ _ c _) = a ! c

>     extend f (DynP a x y c (b1, b2)) = 
>           let es = map (\c' -> (c', f (DynP a x y c' (b1, b2)))) (range (b1, b2))
>               a' = array (b1, b2) es
>           in DynP a' x y c (b1, b2)

> instance Functor (DynP x) where
>     fmap f = extend (f . extract)


Levenshtein edit-distance algorithms

> levenshtein :: DynP Char Int -> Int
> levenshtein = [codo|  _ => -- Initialise first row and column
>                            d    <- levenshtein _
>                            dn   <- (extract d) + 1
>                            d0   <- (constant 0) `fbyX` dn
>                            d'   <- d0 `fbyY` dn
>                            -- Shift (-1, 0), (0, -1), (-1, -1)
>                            d_w  <- d !!! (-1, 0)
>                            d_n  <- d !!! (0, -1)
>                            d_nw <- d !!! (-1, -1)
>                            -- Body
>                            d'' <- if (correspondingX d == correspondingY d) then
>                                      extract d_nw
>                                   else minimum [(extract d_w) + 1,
>                                                 (extract d_n) + 1,
>                                                 (extract d_nw) + 1]
>                            d' `thenXY` d''  |]
      
> edit_distance x y = levenshtein <<= (DynP undefined (' ':x) (' ':y) (0, 0) ((0, 0), (length x, length y)))

Operations on dynamic programming grids

> (!!!) = flip ixRelative

> -- Relative indexing of the grid
> ixRelative :: (Int, Int) -> DynP x a -> a
> ixRelative (x1, x2) (DynP a _ _ c@(c1, c2) _) = a ! (c1 + x1, c2 + x2)

> correspondingX, correspondingY :: DynP x a -> x
> correspondingX (DynP s x y c@(c1, c2) _) = x!!c1
> correspondingY (DynP s x y c@(c1, c2) _) = y!!c2


> fbyX :: DynP x a -> DynP y a -> a
> fbyX (DynP s _ _ c@(c1, c2) _) (DynP s' _ _ c'@(c1', c2') _)
>          = if (c1 == 0 && c1' == 0) then s ! (0, c2)
>            else s' ! (c1' - 1, c2')

> fbyY :: DynP x a -> DynP y a -> a
> fbyY (DynP s _ _ c@(c1, c2) _) (DynP s' _ _ c'@(c1', c2') _)
>          = if (c2 == 0 && c2' == 0) then s ! (c1, 0)
>            else s' ! (c1', c2' - 1)

> thenXY :: DynP x a -> DynP x a -> a
> thenXY (DynP s _ _ c@(c1, c2) _) (DynP s' _ _ c'@(c1', c2') _)
>      = if ((c1 == 0 && c1' == 0) || (c2 == 0 && c2' == 0)) then
>                         s ! (c1, c2)
>                    else s' ! (c1', c2')

> constant :: a -> DynP x a
> constant x = let arr = array ((0, 0), (0, 0)) [((0, 0), x)]
>              in DynP arr [] [] (0, 0) ((0, 0), (0, 0))

Output functions

> instance (Show a) => Show (DynP x a) where
>     show (DynP a x y c ((bx0, by0), (bxn, byn))) =
>         let row v = (show $ map (\u -> a ! (u,v)) [bx0..bxn]) ++ "\n"
>         in concatMap row [by0..byn] -- ("b - " ++ (show ((bx0, by0), (bxn, byn)))) `trace` 

> output :: Show a => DynP Char a -> String
> output (DynP a x y c _) =
>         let top = "  " ++ foldr (\c -> \r -> [c] ++ " " ++ r) "" x ++ "\n"
>             row v = [y!!v] ++ (show $ map (\u -> (a ! (u,v))) [0..(length x - 1)]) ++ "\n"
>         in top ++ concatMap row [0..(length y - 1)]            

> l = "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"

> r = "But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born and I will give you a complete account of the system, and expound the actual teachings of the great explorer of the truth, the master-builder of human happiness. No one rejects, dislikes, or avoids pleasure itself, because it is pleasure, but because those who do not know how to pursue pleasure rationally encounter consequences that are extremely painful. Nor again is there anyone who loves or pursues or desires to obtain pain of itself, because it is pain, but because occasionally circumstances occur in which toil and pain can procure him some great pleasure. To take a trivial example, which of us ever undertakes laborious physical exercise, except to obtain some advantage from it? But who has any right to find fault with a man who chooses to enjoy a pleasure that has no annoying consequences, or one who avoids a pain that produces no resultant pleasure?"


Not used in this example


> prod :: [a] -> [b] -> [(a, b)]
> prod xs ys = xs >>= (\x' -> ys >>= (\y' -> return (x', y')))

> class Comonad c => ComonadZip c where
>     czip ::  (c a, c b) -> c (a, b)

> -- pre condition: the dyn prog paramerters are equal
> instance ComonadZip (DynP x) where
>     czip ((DynP s l t c@(c1, c2) ((bx0, by0), (bxn, byn))),
>           (DynP s' _ _ d@(d1, d2) ((bx0', by0'), (bxn', byn')))) = 
>         let (y1, y2) = (max c1 d1, max c2 d2)
>             es = map (\(x1, x2) -> let c' = (x1 - y1 + c1, x2 - y2 + c2)
>                                        d' = (x1 - y1 + d1, x2 - y2 + d2)
>                                    in ((x1, x2), (s ! c', s' ! d')))                                    
>                                       (prod [(min bx0 bx0')..(max bxn bxn')]
>                                             [(min by0 by0')..(max byn byn')])
>             a' = array ((min bx0 bx0', min by0 by0'), (max bxn bxn', max byn byn')) es
>         in   DynP a' l t (y1, y2) ((min bx0 bx0', min by0 by0'), (max bxn bxn', max byn byn'))