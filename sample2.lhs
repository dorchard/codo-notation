> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE QuasiQuotes #-}               
> import Control.Comonad
> import Language.Haskell.Codo
> import Data.Monoid

> instance Monoid Double where
>     mempty = 0.0
>     mappend = (+)

> differentiate f = ((f 0.001) - f 0) / 0.001

> minima :: (Double -> Double) -> Bool
> minima = [codo| f => f'  <- differentiate f
>                      f'' <- differentiate f'
>                      (extract f' < 0.001) && (extract f'' > 0) |] 

