> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> import Language.Haskell.Codo
> import Control.Comonad
> import Data.Monoid
> import Data.Array
> import Text.Printf

> import Context

> import System.IO

Monoid for the domain of the exponent comonad

> instance Monoid Float where
>     mempty = 0
>     mappend = (+)

Differentiate function

> differentiate :: (Float -> Float) -> Float
> differentiate f = (f ep - f 0.0) / ep where ep = 0.01



Minima testing function

> roughlyEqual x y = x < (y+0.0000001) && x > (y-0.0000001)

> minima = [codo| f => f' <- differentiate f
>                      f'' <- differentiate f'
>                      (extract f'' `roughlyEqual` 0) && (extract f'' < 0) |]

Macluarin approximations

> m3 = [codo| (f, x) => f' <- differentiate f
>                       f'' <- differentiate f'
>                       (f (-extract x))
>                        + (f' (-extract x)) * (extract x)
>                        + (f'' (-extract x)) / 2 * (extract x)**2 |]

> m3' = [codo| (f, xf) => f' <- differentiate f
>                         f'' <- differentiate f'
>                         let x = extract xf
>                         (f (-x)) 
>                          + (f' (-x)) * x
>                          + ((f'' (-x)) / 2) * x**2 |]

Zipping operations

> class Comonad c => ComonadZip c where
>     czip ::  (c a, c b) -> c (a, b)

> instance Monoid x => ComonadZip ((->) x) where
>     czip (f, g) = \x -> (f x, g x)


Example curves

> circF :: Float -> Float
> circF x = sqrt (1 - x*x)
>               
> lineF :: Float -> Float -> (Float -> Float)
> lineF m c x = m*x + c

> expF :: Float -> Float
> expF = exp

Output functions

> instance Show a => Show (Float -> a) where
>     show c = concatMap (\i -> show i ++ " = " ++ show (c i) ++ "\n") [-2,-1.75..2]


Output to .dat file for gnuplot

> plotGraph f = concatMap (\i -> if (isNaN (f i)) then printf "\n" else printf "%.3f %.3f\n" (f i) i) [-2,-1.99..2]

> plot f file = writeFile file (plotGraph f)

> plotGraph2 f g = concatMap (\i -> if (isNaN (f i) || isNaN (g i)) then printf "\n" else printf "%.3f \t %.3f \t %.3f\n" i (f i) (g i)) [-2,-1.999..2]
> plot2 x y f = writeFile f ("# \"x\" \t \"f x\" \t \"g x\"\n" ++ plotGraph2 x y)


e.g. 

> foo1 = plot2 expF (m3 <<= (czip (expF, id))) "exp-approx.dat"

gnuplot> set style data lines
gnuplot> plot "exp-approx.dat" using 1:2 title 'exp', "exp-approx.dat" using 1:3 title 'exp-approx'
