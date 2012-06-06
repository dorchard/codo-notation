> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Language.Haskell.Codo where

> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr
> import Text.Parsec.Char
> import qualified Text.ParserCombinators.Parsec.Token as Token

> import Data.Generics.Uniplate.Data

> import Language.Haskell.TH 
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Quote
> import Language.Haskell.Meta.Parse

> import Data.Maybe
> import Debug.Trace
> import Data.Char

> import Control.Comonad.Alt

> fv var = varE $ mkName var

> doParse :: Monad m => (Parser a) -> (String, Int, Int) -> String -> m a
> doParse parser (file, line, col) input =
>     case (runParser p () "" input) of
>          Left err  -> fail $ show err
>          Right x   -> return x
>          where
>           p = do { pos <- getPosition;
>                    setPosition $
>                     (flip setSourceName) file $
>                     (flip setSourceLine) line $
>                     (flip setSourceColumn) col $ pos;
>                    x <- parser;
>                    return x; }
              
> pattern  = (try ( do string "=>"
>                      return "" )) <|>
>                ( do p <- anyChar
>                     ps <- pattern 
>                     return $ p:ps )


> codoTransPart = do s1 <- many space
>                    p <- pattern
>                    rest <- many (codoTransPart')
>                    return $ (take (length s1 - 4) (repeat ' '))
>                               ++ "\\" ++ p ++ "-> do" ++ concat rest

> codoTransPart' = try ( do string "codo"
>                           s1 <- many space
>                           p <- pattern
>                           s3 <- many space
>                           pos <- getPosition
>                           col <- return $ sourceColumn pos
>                           marker <- return $ ("_reserved_codo_block_marker_\n" ++ (take (col - 1) (repeat ' ')))
>                           return $ "\\" ++ p ++ "->" ++ s1 ++ "do " ++ s3 ++ marker)
>                  <|> ( do c <- anyChar
>                           if c=='_' then return "gamma"
>                            else return [c] )

> -- Note all these functions for making binders take a variable which is the "gamma" variable

> mkProjBind :: Name -> ExpQ -> ExpQ -> DecQ
> mkProjBind var prj gam = valD (varP $ var) (normalB ([| cmap $(prj) $(gam) |])) []

> projs :: [Name] -> ExpQ -> [DecQ]
> projs x gam = projs' x [| id |] gam

> projs' :: [Name] -> ExpQ -> ExpQ -> [DecQ]
> projs' [] _ _ = []
> projs' [x] l gam = --if (showName x == "gamma") then 
>                --    [valD (varP x) (normalB [| $(varE x) |]) []]
>                --else 
>                    [valD (varP x) (normalB gam) []]
> projs' [x, y] l gam = [mkProjBind x [| snd . $(l) |] gam, mkProjBind y [| fst . $(l) |] gam]
> projs' (x:xs) l gam = (mkProjBind x [| snd . $(l) |] gam):(projs' xs [| $(l) . fst |] gam)

> envProj vars exp = let gam = mkName "gamma" in (lamE [varP gam] (letE (projs vars (varE gam)) exp))

> expToPat :: Exp -> Pat
> expToPat (VarE v) = VarP v
> expToPat (TupE xs) = TupP (map expToPat xs)
> expToPat e = error $ "Codo cannot have the following pattern on LHS of a binder " ++ show e

> patToBinders :: Pat -> [Name]
> patToBinders (VarP v) = [v]
> patToBinders (TupP xs) = concatMap patToBinders xs
> patToBinders p = error $ "Codo cannot have the following pattern on LHS of a binder " ++ show p

> codoBind :: [Stmt] -> [Name] -> Q Exp
> codoBind [NoBindS e]             vars = case e of 
>                                           UInfixE e1 (VarE v) e2 | showName v == "<<-" ->
>                                             error "Codo block must end with an expression"
>                                           _ -> [| \gamma -> $(envProj vars (transformM (doToCodo) e)) gamma |]
> codoBind [x]                     vars = error "Codo block must end with an expressions"      
> codoBind ((NoBindS e):bs)        vars = case e of 
>                                           UInfixE e1 (VarE v) e2 | showName v == "<<-" -> 
>                                               let p = expToPat e1
>                                                   vs = patToBinders p
>                                               in    [| $(codoBind bs (vs++vars)) . 
>                                                            (cobind
>                                                              (\gamma -> 
>                                                              (coreturn gamma,
>                                                             $(transformM doToCodo e2) gamma))) |]
>                                           _ -> [| $(codoBind bs vars) .
>                                                    (cobind (\gamma ->
>                                                      (coreturn gamma,
>                                                        $(envProj vars (transformM (doToCodo) e)) gamma))) |]

> codoBind ((LetS [ValD (VarP v) (NormalB e) []]):bs) vars = 
>                                           [| (\gamma -> 
>                                                  $(letE [valD (varP $ v)
>                                                   (normalB $ [| $(envProj vars (transformM (doToCodo) e)) gamma |]) []] [| $(codoBind bs vars) $(fv "gamma") |])) |]


> codoBind ((BindS (VarP v) e):bs) vars = [| $(codoBind bs (v:vars)) . 
>                                            (cobind (\gamma ->
>                                                       (coreturn gamma, 
>                                                        $(envProj vars (transformM (doToCodo) e)) gamma))) |]
> codoBind _ _ = error "Ill-formed codo bindings"

> doToCodo :: Exp -> Q Exp
> doToCodo (LamE [VarP v] (DoE ((NoBindS (VarE n)):stmts)))
>              | showName n == "_reserved_codo_block_marker_" = codoBind stmts [v] 
>                                 -- notably, doesn't pick up outside environment
>              | otherwise = return $ (DoE ((NoBindS (VarE n)):stmts))
> doToCodo e  = return e


> codoPatternError = "Malformed codo: codo must start with either a variable, wildcard, or tuple pattern (of wildcards or variables)"

> codoMain :: Exp -> Q Exp
> codoMain (LamE [TupP ps] (DoE stms)) = let p (VarP v) = v
>                                            p (WildP) = mkName "gamma"
>                                            p _ = error codoPatternError
>                                        in codoBind stms (reverse $ map p ps)
> codoMain (LamE [WildP] (DoE stms)) = codoBind stms [mkName "gammaWild"]
> codoMain (LamE [VarP v] (DoE stms)) = codoBind stms [v]
> codoMain _ = error codoPatternError

> interpretCodo s = do loc <- location                      
>                      let pos = (loc_filename loc,
>                                   fst (loc_start loc),
>                                   1) -- set to 1 as we add spaces in to account for
>                                      -- the start of the line
>                      -- the following corrects the text to account for the preceding
>                      -- Haskell code + quasiquote, to preserve alignment of further lines
>                      s'' <- return ((take (snd (loc_start loc) - 1) (repeat ' ')) ++ s)
>                      s''' <- (doParse codoTransPart pos s'')                 
>                      case (s''' `trace` parseExp s''') of
>                             Left l -> error l
>                             Right e -> codoMain e


> codo :: QuasiQuoter
> codo = QuasiQuoter { quoteExp = interpretCodo,
>                      quotePat = undefined,
>                      quoteType = undefined,
>                      quoteDec = undefined }


> bindors :: Comonad c => c [a] -> [c a]

> bindors x = (cmap head x) : (bindors (cmap tail x))