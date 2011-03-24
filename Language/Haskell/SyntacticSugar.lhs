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

> codo :: QuasiQuoter
> codo = QuasiQuoter { quoteExp = interpretBlock parseBlock interpretCoDo,
>                      quotePat = (\_ -> wildP) --,
>                       -- quoteType = undefined,
>                       -- quoteDec = undefined
>                     }

> interpretCoDo :: Block -> Maybe (Q Exp)
> interpretCoDo (Block var binds) =
>     do inner <- interpretCobinds binds var
>        Just $ lamE [varP $ mkName var] inner

> interpretCobinds :: Binds -> Variable -> Maybe (Q Exp)
> interpretCobinds (EndExpr exp) _ =
>      case parseToTH exp of
>             Left x -> error x
>             Right exp' -> Just $ return exp'
> interpretCobinds (Bind var exp binds) outerVar = 
>      case parseToTH exp of
>         Left x -> error x
>         Right exp' ->
>             do let coKleisli = lamE [varP $ mkName outerVar] (return exp')
>                inner <- (interpretCobinds binds var)
>                let remaining = lamE [varP $ mkName var] inner -- (appE inner (varE $ mkName var))
>                return [| ($(remaining) . ($(free "coextend") $(coKleisli)))
>                          $(free outerVar) |]

> bido :: QuasiQuoter
> bido = QuasiQuoter { quoteExp = interpretBlock parseBlock interpretBiDo,
>                      quotePat = (\_ -> wildP) --,
>                       -- quoteType = undefined,
>                       -- quoteDec = undefined
>                     }

> interpretBiDo :: Block -> Maybe (Q Exp)
> interpretBiDo (Block var binds) = 
>     do inner <- interpretBiBinds binds var
>        Just $ lamE [varP $ mkName var] inner

> interpretBiBinds :: Binds -> Variable -> Maybe (Q Exp)
> interpretBiBinds (EndExpr exp) _ =
>     case parseToTH exp of
>            Left x -> error x
>            Right exp' -> Just $ return exp'
> interpretBiBinds (Bind var exp binds) outerVar =
>     case parseToTH exp of
>        Left x -> error x
>        Right exp' ->
>            do let biKleisli = lamE [varP $ mkName outerVar] (return exp')
>               inner <- (interpretBiBinds binds var)
>               let remaining = lamE [varP $ mkName var] inner
>               return [| (($(free "fmap") $(free "counit")) .
>                         ($(free "biextend") $(remaining)) .
>                         ($(free "biextend") $(biKleisli)) .
>                         ($(free "return"))) $(free outerVar) |]

> pair :: (a -> b, a -> c) -> a -> (b, c)
> pair (f, g) = \x -> (f x, g x)

> cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
> cross f g (x, y) = (f x, g y)

> codo2 :: QuasiQuoter
> codo2 = QuasiQuoter { quoteExp = interpretBlock parseBlock interpretCoDo2,
>                       quotePat = (\_ -> wildP) --,
>                       -- quoteType = undefined,
>                       -- quoteDec = undefined
>                     }

> interpretCoDo2 :: Block -> Maybe (Q Exp)
> interpretCoDo2 (Block var binds) =
>     do inner <- interpretCobinds2 binds [var]
>        Just $ lamE [varP $ mkName var] (appE inner (varE $ mkName var))

> recursiveTuples [v] = varP $ mkName v
> recursiveTuples [v,v'] = tupP [varP $ mkName v, varP $ mkName v']
> recursiveTuples (v:vs) = tupP [varP $ mkName v, recursiveTuples vs]

> recursiveUnpack [_] = [| id |]
> recursiveUnpack (_:vs) = [| $(unpackN (length (vs) + 1)) . $(recursiveUnpack vs) |]

> unpackN 1 = [| id |]
> unpackN 2 = [| $(free "m_op") |]
> unpackN n = [| (id `cross` $(unpackN (n-1))) |]

> interpretCobinds2 :: Binds -> [Variable] -> Maybe (Q Exp)
> interpretCobinds2 (EndExpr exp) binders =
>      case parseToTH exp of
>             Left x -> error x
>             Right exp' -> do let pattern = recursiveTuples binders
>                              Just $ [| $(lamE [pattern] (return exp')) . $(recursiveUnpack binders) |]
> interpretCobinds2 (Bind var exp binds) binders = 
>      case parseToTH exp of
>         Left x -> error x
>         Right exp' ->
>             do 
>                let binders' = var:binders
>                let pattern = recursiveTuples binders
>                let coKleisli = [| $(lamE [pattern] (return exp')) . $(recursiveUnpack binders) |]
>                inner <- (interpretCobinds2 binds binders')
>                return [| ($(inner) . $(free "m") . (pair ($(free "coextend") $(coKleisli), id))) |]

> codo3 :: QuasiQuoter
> codo3 = QuasiQuoter { quoteExp = interpretBlock parseBlock interpretCoDo3,
>                       quotePat = (\_ -> wildP) --,
>                       -- quoteType = undefined,
>                       -- quoteDec = undefined
>                     }

> interpretCoDo3 :: Block -> Maybe (Q Exp)
> interpretCoDo3 (Block var binds) =
>     do inner <- interpretCobinds3 binds [var]
>        Just $ lamE [varP $ mkName var] (appE inner (varE $ mkName var))
> interpretCobinds3 :: Binds -> [Variable] -> Maybe (Q Exp)
> interpretCobinds3 (EndExpr exp) binders =
>      case parseToTH exp of
>             Left x -> error x
>             Right exp' -> do let pattern = recursiveTuples binders
>                              Just $ [| $(lamE [pattern] (return exp')) . $(recursiveUnpack binders) |]
> interpretCobinds3 (Bind var exp binds) binders = 
>      case parseToTH exp of
>         Left x -> error x
>         Right exp' ->
>             do 
>                let binders' = var:binders
>                let pattern = recursiveTuples binders
>                let coKleisli = [| $(lamE [pattern] (return exp')) . $(recursiveUnpack binders) |]
>                inner <- (interpretCobinds3 binds binders')
>                return [| $(inner) . ($(free "coextend") (pair ($(coKleisli), $(free "coreturn")))) |]
