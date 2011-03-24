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

> pair :: (a -> b, a -> c) -> a -> (b, c)
> pair (f, g) = \x -> (f x, g x)

> cross :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
> cross f g (x, y) = (f x, g y)

> recursiveTuples [v] = varP $ mkName v
> recursiveTuples [v,v'] = tupP [varP $ mkName v, varP $ mkName v']
> recursiveTuples (v:vs) = tupP [varP $ mkName v, recursiveTuples vs]

> recursiveUnpack [_] = [| id |]
> recursiveUnpack (_:vs) = [| $(unpackN (length (vs) + 1)) . $(recursiveUnpack vs) |]

> m_op = [| \s -> ($(free "cmap") fst s, $(free "cmap") snd s) |]

> unpackN 1 = [| id |]
> unpackN 2 = [| $(m_op) |]
> unpackN n = [| (id `cross` $(unpackN (n-1))) |]

> interpretCoDo :: Block -> Maybe (Q Exp)
> interpretCoDo (Block var binds) =
>     do inner <- interpretCobinds binds [var]
>        Just $ lamE [varP $ mkName var] (appE inner (varE $ mkName var))
> interpretCobinds :: Binds -> [Variable] -> Maybe (Q Exp)
> interpretCobinds (EndExpr exp) binders =
>      case parseToTH exp of
>             Left x -> error x
>             Right exp' -> do let pattern = recursiveTuples binders
>                              Just $ [| $(lamE [pattern] (return exp')) . $(recursiveUnpack binders) |]
> interpretCobinds (Bind var exp binds) binders = 
>      case parseToTH exp of
>         Left x -> error x
>         Right exp' ->
>             do 
>                let binders' = var:binders
>                let pattern = recursiveTuples binders
>                let coKleisli = [| $(lamE [pattern] (return exp')) . $(recursiveUnpack binders) |]
>                inner <- (interpretCobinds binds binders')
>                return [| $(inner) . ($(free "coextend") (pair ($(coKleisli), $(free "coreturn")))) |]


 bido :: QuasiQuoter
 bido = QuasiQuoter { quoteExp = interpretBlock parseBlock interpretBiDo,
                      quotePat = (\_ -> wildP) --,
                       -- quoteType = undefined,
                       -- quoteDec = undefined
                     }

 interpretBiDo :: Block -> Maybe (Q Exp)
 interpretBiDo (Block var binds) = 
     do inner <- interpretBiBinds binds var
        Just $ lamE [varP $ mkName var] inner

 interpretBiBinds :: Binds -> Variable -> Maybe (Q Exp)
 interpretBiBinds (EndExpr exp) _ =
     case parseToTH exp of
            Left x -> error x
            Right exp' -> Just $ return exp'
 interpretBiBinds (Bind var exp binds) outerVar =
     case parseToTH exp of
        Left x -> error x
        Right exp' ->
            do let biKleisli = lamE [varP $ mkName outerVar] (return exp')
               inner <- (interpretBiBinds binds var)
               let remaining = lamE [varP $ mkName var] inner
               return [| (($(free "fmap") $(free "counit")) .
                         ($(free "biextend") $(remaining)) .
                         ($(free "biextend") $(biKleisli)) .
                         ($(free "return"))) $(free outerVar) |]

