> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE QuasiQuotes #-}               
> import Control.Comonad
> import Language.Haskell.Codo
>
> foo :: (Comonad c, Num a) => c a -> a
> foo = [codo| x => extract x + 1 |] 