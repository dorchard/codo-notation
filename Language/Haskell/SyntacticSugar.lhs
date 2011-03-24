> {-# LANGUAGE TemplateHaskell #-}

> module Language.Haskell.SyntacticSugar where

> import Text.ParserCombinators.Parsec
> import Language.Haskell.SyntacticSugar.Parse
> import Language.Haskell.TH 
> import Language.Haskell.TH.Quote
> import Language.Haskell.SyntaxTrees.ExtsToTH
> import Data.Generics

> free var = varE $ mkName var

> interpretBlock :: Parser Block -> (Block -> Maybe (Q Exp)) -> String -> Q Exp
> interpretBlock parse interpret input = 
>                        do loc <- location
>                           let pos = (loc_filename loc,
>                                      fst (loc_start loc), 
>                                      snd (loc_start loc))
>                           expr <- (parseExpr parse) pos input
>                           dataToExpQ (const Nothing `extQ` interpret) expr

> pair :: (a -> b, a -> c) -> a -> (b, c)
> pair (f, g) = \x -> (f x, g x)


> codo :: QuasiQuoter
> codo = QuasiQuoter { quoteExp = interpretBlock parseBlock interpretCoDo,
>                      quotePat = (\_ -> wildP) --,
>                       -- quoteType = undefined,
>                       -- quoteDec = undefined
>                     }

> mkProjBind :: String -> ExpQ -> DecQ
> mkProjBind var prj = valD (varP $ mkName var) (normalB ([| $(free "cmap") $(prj) $(free "gamma") |])) []

> projs :: [String] -> [DecQ]
> projs x = projs' x [| id |]

> projs' :: [String] -> ExpQ -> [DecQ]
> projs' [] _ = []
> projs' [x] l = [valD (varP $ mkName x) (normalB [| $(free "gamma") |]) []]
> projs' [x, y] l = [mkProjBind x [| fst . $(l) |], mkProjBind y [| snd . $(l) |]]
> projs' (x:xs) l = (mkProjBind x [| fst . $(l) |]):(projs' xs [| $(l) . snd |])

> interpretCoDo :: Block -> Maybe (Q Exp)
> interpretCoDo (Block var binds) =
>     do inner <- interpretCobinds binds [var]
>        Just $ lamE [varP $ mkName var] (appE inner (varE $ mkName var))
> interpretCobinds :: Binds -> [Variable] -> Maybe (Q Exp)
> interpretCobinds (EndExpr exp) binders =
>      case parseToTH exp of
>             Left x -> error x
>             Right exp' -> Just $ (lamE [varP $ mkName "gamma"] (letE (projs binders) (return exp')))
> interpretCobinds (Bind var exp binds) binders = 
>      case parseToTH exp of
>         Left x -> error x
>         Right exp' ->
>             do 
>                let binders' = var:binders
>                let coKleisli = lamE [varP $ mkName "gamma"] (letE (projs binders) (return exp'))
>                inner <- (interpretCobinds binds binders')
>                return [| $(inner) . ($(free "cobind") (pair ($(coKleisli), $(free "coreturn")))) |]


> -- Derive a bunch of combinators that we need

> bind :: Monad m => (a -> m b) -> m a -> m b
> bind = flip (>>=)

> mstrength :: Monad m => (a, m b) -> m (a, b)
> mstrength (a, mb) = mb >>= (\b -> return (a, b))

> mstrength' :: Monad m => (m a, b) -> m (a, b)
> mstrength' (ma, b) = ma >>= (\a -> return (a, b))

> -- We use biextension as it is more basic than composition

> bido :: QuasiQuoter
> bido = QuasiQuoter { quoteExp = interpretBlock parseBlock interpretBiDo,
>                      quotePat = (\_ -> wildP) --,
>                       -- quoteType = undefined,
>                       -- quoteDec = undefined
>                     }

> interpretBiDo :: Block -> Maybe (Q Exp)
> interpretBiDo (Block var binds) = 
>     do inner <- interpretBiBinds binds [var]
>        Just $ lamE [varP $ mkName var] [| (($(free "fmap") $(free "coreturn")) .
>                                            $(inner) . ($(free "return"))) $(free var) |]

> interpretBiBinds :: Binds -> [Variable] -> Maybe (Q Exp)

> interpretBiBinds (EndExpr exp) binders =
>     case parseToTH exp of
>        Left x -> error x
>        Right exp' ->
>             do let biKleisli = lamE [varP $ mkName "gamma"] (letE (projs binders) (return exp'))
>                return [| $(free "bibind") $(biKleisli) |]

> interpretBiBinds (Bind var exp binds) binders =
>     case parseToTH exp of
>        Left x -> error x
>        Right exp' ->
>             do let binders' = var:binders
>                let biKleisli = lamE [varP $ mkName "gamma"] (letE (projs binders) (return exp'))
>                inner <- (interpretBiBinds binds binders')
>                return [| ($(inner)) .
>                          ($(free "bibind") ($(free "mstrength'") .
>                                             (pair ($(biKleisli), $(free "coreturn"))))) |]

