> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Language.Haskell.Codo(codo,context,coextendR) where -- coextendR is only exported at the moment for illustration purposes but later should be hidden

> import Text.ParserCombinators.Parsec

> import Control.Lens
> import Data.Data.Lens

> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Quote
> import Language.Haskell.Meta.Parse

> import Data.Functor(void)

> import Control.Comonad

Optimisation re-write rules - changes complexity from O(n^k) to O(n) for codo

> {-# RULES "coextend/assoc" forall g f . coextendR (g . coextendR f) = coextendR g . coextendR f #-}

> {-# RULES "coextend/cmap/assoc" forall h g f . coextendR ((g . coextendR f) . cmapR h) = coextendR g . coextendR f . cmapR h #-}

For GHC rewriting to work it requires inlined-aliases to the
core operations of the comonad

> {-# INLINE cmapR #-}
> cmapR :: Functor c => (a -> b) -> c a -> c b
> cmapR = fmap

> {-# INLINE coextendR #-}
> coextendR :: Comonad c => (c a -> b) -> c a -> c b
> coextendR = extend

> fv :: String -> ExpQ
> fv var = varE $ mkName var

> -- Codo translation comprises a (1) parsing/textual-transformation phase
> --                              (2) interpretation phase
> --                                    i). top-level transformation
> --                                    ii). bindings transformations
> --                                    iii). expression transformation

> -- *****************************
> -- (1) Parsing/textual-transformation
> -- *****************************

> context :: QuasiQuoter
> context = codo

> codo :: QuasiQuoter
> codo = QuasiQuoter { quoteExp = interpretCodo,
>                      quotePat = undefined,
>                      quoteType = undefined,
>                      quoteDec = undefined }


> interpretCodo :: String -> Q Exp
> interpretCodo s = do loc <- location
>                      let pos = (loc_filename loc,
>                                   fst (loc_start loc),
>                                   1) -- set to 1 as we add spaces in to account for
>                                      -- the start of the line
>                      -- the following corrects the text to account for the preceding
>                      -- Haskell code + quasiquote, to preserve alignment of further lines
>                      let s'' = replicate (snd (loc_start loc) - 1) ' ' ++ s
>                      s''' <- doParse codoTransPart pos s''
>                      case parseExp s''' of
>                             Left l -> error l
>                             Right e -> codoMain e

> doParse :: MonadFail m => Parser a -> (String, Int, Int) -> String -> m a
> doParse parser (file, line, col) input =
>     case runParser p () "" input of
>          Left err  -> fail $ show err
>          Right x   -> return x
>          where
>           p = do { pos <- getPosition;
>                    setPosition $
>                     flip setSourceName file $
>                     flip setSourceLine line $
>                     setSourceColumn pos col;
>                    x <- parser;
>                    return x; }


> -- Parsing a codo-block

> pattern_ :: Parser String
> pattern_  = try ( "" <$ string "=>") <|>
>   ((:) <$> anyChar <*> pattern_)



> codoTransPart :: Parser String
> codoTransPart = do s1 <- many space
>                    p <- pattern_
>                    rest <- many codoTransPart'
>                    return $ replicate (length s1 - 4) ' '
>                               ++ "\\" ++ p ++ "-> do" ++ concat rest

> codoTransPart' :: Parser String
> codoTransPart' = try ( do void $ string "codo" 
>                           s1 <- many space
>                           p <- pattern_
>                           s3 <- many space
>                           pos <- getPosition
>                           let col = sourceColumn pos
>                               marker = "_reserved_codo_block_marker_\n" ++ replicate (col - 1) ' '
>                           return $ "\\" ++ p ++ "->" ++ s1 ++ "do " ++ s3 ++ marker)
>                  <|> ( do c <- anyChar
>                           if c=='_' then return "_reserved_gamma_"
>                            else return [c] )

> -- *****************************
> -- (2) interpretation phase
> -- *****************************
> --   i). top-level transformation
> -- *****************************

> -- Top-level translation
> codoMain :: Exp -> Q Exp
> codoMain (LamE p bs) = [| $(codoMain' (LamE p bs)) . cmapR $(return $ projFun p) |]

> codoMain' :: Exp -> Q Exp
> codoMain' (LamE [TupP ps] (DoE stms)) = codoBind stms (concatMap patToVarPs ps)
> codoMain' (LamE [WildP] (DoE stms)) = codoBind stms [mkName "_reserved_gamma_"]
> codoMain' (LamE [VarP v] (DoE stms)) = codoBind stms [v]
> codoMain' _ = error codoPatternError

> codoPatternError :: String
> codoPatternError = "Malformed codo: codo must start with either a variable, wildcard, or tuple pattern (of wildcards or variables)"

> -- create the projection function to arrange the codo-Block parameters into the correct ordder
> patToVarPs :: Pat -> [Name]
> patToVarPs (TupP ps) = concatMap patToVarPs ps
> patToVarPs WildP     = [mkName "_reserved_gamma_"]
> patToVarPs (VarP v)  = [v]
> patToVarPs _         = error "Only tuple, variable, or wildcard patterns currently allowed"

> projExp :: [Exp] -> Exp
> projExp [] = TupE []
> projExp (x:xs) = TupE [x, projExp xs]

> projPat :: [Pat] -> Pat
> projPat [] = TupP []
> projPat (x:xs) = TupP [x, projPat xs]

> projFun :: [Pat] -> Exp
> projFun p = LamE (map replaceWild p) (projExp (map VarE (concatMap patToVarPs p)))

> replaceWild :: Pat -> Pat
> replaceWild WildP = VarP $ mkName "_reserved_gamma_"
> replaceWild x = x

> -- **********************
> -- ii). bindings transformations
> -- **********************

> convert :: [Name] -> [Name] -> Exp
> convert lVars envVars = LamE [TupP [TupP (map VarP lVars),
>                                     projPat (map VarP envVars)]] (projExp (map VarE (lVars ++ envVars)))

> -- Note all these functions for making binders take a variable which is the "gamma" variable
> -- Binding interpretation (\vdash_c)

> codoBind :: [Stmt] -> [Name] -> Q Exp
> codoBind [NoBindS e]             vars = [| $(envProj vars (transformMOf uniplate doToCodo e)) |]
> codoBind [_]                     _    = error "Codo block must end with an expressions"
> codoBind ((NoBindS e):bs)        vars = [| $(codoBind bs vars) .
>                                               (coextendR (\gamma ->
>                                                  ($(envProj vars (transformMOf uniplate doToCodo e)) gamma,
>                                                   extract gamma))) |]

> codoBind ((LetS [ValD p (NormalB e) []]):bs) vars =
>                                           [| (\gamma ->
>                                                  $(letE [valD (return p)
>                                                   (normalB $ [| $(envProj vars (transformMOf uniplate doToCodo e)) gamma |]) []] [| $(codoBind bs vars) $(fv "gamma") |])) |]

> codoBind ((BindS (VarP v) e):bs) vars = [| $(codoBind bs (v:vars)) .
>                                            (coextendR (\gamma ->
>                                                       ($(envProj vars (transformMOf uniplate doToCodo e)) gamma,
>                                                        extract gamma))) |]
> codoBind ((BindS (TupP ps) e):bs) vars = [| $(codoBind bs (concatMap patToVarPs ps ++ vars)) .
>                                            (coextendR (\gamma ->
>                                                      $(return $ convert (concatMap patToVarPs ps) vars)  
>                                                       ($(envProj vars (transformMOf uniplate doToCodo e)) gamma,
>                                                        extract gamma))) |]
> codoBind t _ = error "Ill-formed codo bindings"

> doToCodo :: Exp -> Q Exp
> doToCodo (LamE [VarP v] (DoE ((NoBindS (VarE n)):stmts)))
>              -- Nested codo-block
>              -- notably, doesn't pick up outside environment
>              | showName n == "_reserved_codo_block_marker_" = codoMain (LamE [VarP v] (DoE stmts))
>
>              | otherwise = return $ DoE (NoBindS (VarE n):stmts)
> doToCodo e  = return e


> -- ***********************
> --  iii). expression transformation
> -- ***********************

> -- Creates a scope where all the local variables are project
> envProj :: [Name] -> ExpQ -> ExpQ
> envProj vars expq = let gam = mkName "gamma" in lamE [varP gam] (letE (projs vars (varE gam)) expq)

> -- Make a comonadic projection
> mkProj :: ExpQ -> Name -> Int -> DecQ
> mkProj gam v n = valD (varP v) (normalB [| cmapR $(prj n) $(gam) |]) []

> -- Creates a list of projections
> projs :: [Name] -> ExpQ -> [DecQ]
> projs x gam = zipWith (mkProj gam) x [0..(length x - 1)]

> -- Computes the correct projection
> prj :: Int -> ExpQ
> prj 0 = [| fst |]
> prj n = [| $(prj (n-1)) . snd |]
