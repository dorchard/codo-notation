> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> module Language.Haskell.Codo(codo,context,coextendR) where -- coextendR is only exported at the moment for illustration purposes but later should be hidden

> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr
> import Text.Parsec.Char
> import qualified Text.ParserCombinators.Parsec.Token as Token

> import Control.Lens
> import Data.Data.Lens

> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Quote
> import Language.Haskell.Meta.Parse hiding (parseExp)
> import Language.Haskell.Meta.Syntax.Translate
> import Language.Haskell.Exts.Parser hiding (parseExp)
> import Language.Haskell.Exts.Extension

> import Data.Maybe
> import Debug.Trace
> import Data.Char

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

> fv var = varE $ mkName var

> -- Codo translation comprises a (1) parsing/textual-transformation phase
> --                              (2) interpretation phase
> --                                    i). top-level transformation
> --                                    ii). bindings transformations
> --                                    iii). expression transformation

> -- *****************************
> -- (1) Parsing/textual-transformation
> -- *****************************

> context = codo

> codo :: QuasiQuoter
> codo = QuasiQuoter { quoteExp = interpretCodo,
>                      quotePat = undefined,
>                      quoteType = undefined,
>                      quoteDec = undefined }


> interpretCodo s = do loc <- location
>                      let pos = (loc_filename loc,
>                                   fst (loc_start loc),
>                                   1) -- set to 1 as we add spaces in to account for
>                                      -- the start of the line
>                      -- the following corrects the text to account for the preceding
>                      -- Haskell code + quasiquote, to preserve alignment of further lines
>                      s'' <- return ((take (snd (loc_start loc) - 1) (repeat ' ')) ++ s)
>                      s''' <- (doParse codoTransPart pos s'')
>                      case (parseExp s''') of
>                             Left l -> error l
>                             Right e -> codoMain e

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


> -- Parsing a codo-block

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
>                           if c=='_' then return "_reserved_gamma_"
>                            else return [c] )

> -- haskell-src-exts settings.

> parseExp = either Left (Right . toExp)
>            . parseResultToEither
>            . parseExpWithMode codoParseMode
>     where
>       codoParseMode = ParseMode
>         { parseFilename         = []
>         , baseLanguage          = Haskell2010
>         , extensions            = map EnableExtension languageExtensions
>         , ignoreLinePragmas     = False
>         , ignoreLanguagePragmas = False
>         , fixities              = Nothing }
>
>       languageExtensions = [ PostfixOperators
>                            , QuasiQuotes
>                            , UnicodeSyntax
>                            , PatternSignatures
>                            , MagicHash
>                            , ForeignFunctionInterface
>                            , TemplateHaskell
>                            , RankNTypes
>                            , MultiParamTypeClasses
>                            , RecursiveDo
>                            , ViewPatterns ]

> -- *****************************
> -- (2) interpretation phase
> -- *****************************
> --   i). top-level transformation
> -- *****************************

> -- Top-level translation
> codoMain :: Exp -> Q Exp
> codoMain (LamE p bs) = [| $(codoMain' (LamE p bs)) . (cmapR $(return $ projFun p)) |]

> codoMain' :: Exp -> Q Exp
> codoMain' (LamE [TupP ps] (DoE stms)) = codoBind stms (zip (ps >>= patToVarPs) (repeat Nothing))
> codoMain' (LamE [WildP]   (DoE stms)) = codoBind stms [(mkName "_reserved_gamma_", Nothing)]
> codoMain' (LamE [VarP v]  (DoE stms)) = codoBind stms [(v, Nothing)]
> codoMain' _                           = error codoPatternError

> codoPatternError = "Malformed codo: codo must start with either a variable, wildcard, or tuple pattern (of wildcards or variables)"

> -- create the projection function to arrange the codo-Block parameters into the correct ordder
> patToVarPs :: Pat -> [Name]
> patToVarPs (TupP ps) = concatMap patToVarPs ps
> patToVarPs WildP     = [mkName "_reserved_gamma_"]
> patToVarPs (VarP v)  = [v]
> patToVarPs _         = error "Only tuple, variable, or wildcard patterns currently allowed"

> projExp [] = TupE []
> projExp (x:xs) = TupE [x, (projExp xs)]

> projPat [] = TupP []
> projPat (x:xs) = TupP [x, (projPat xs)]

> projFun p = LamE (map replaceWild p) (projExp (map VarE (concatMap patToVarPs p)))

> replaceWild WildP = VarP $ mkName "_reserved_gamma_"
> replaceWild x = x

> -- **********************
> -- ii). bindings transformations
> -- **********************

> convert lVars envVars = LamE [TupP [TupP (map VarP lVars),
>                                     projPat (map VarP envVars)]] (projExp (map VarE (lVars ++ envVars)))

> -- Note all these functions for making binders take a variable which is the "gamma" variable
> -- Binding interpretation (\vdash_c)

> codoBind :: [Stmt]
>          -> [(Name, Maybe ExpQ)]  -- ^ A mapping from names to associated view patterns.
>          -> Q Exp
> codoBind [NoBindS e]             vars = [| \gamma -> $(envProj vars (transformMOf uniplate (doToCodo) e)) gamma |]
> codoBind [x]                     vars = error "Codo block must end with an expressions"
> codoBind ((NoBindS e):bs)        vars = [| $(codoBind bs vars) .
>                                               (coextendR (\gamma ->
>                                                  ($(envProj vars (transformMOf uniplate (doToCodo) e)) gamma,
>                                                   extract gamma))) |]

> codoBind ((LetS [ValD p (NormalB e) []]):bs) views =
>                                           [| (\gamma ->
>                                                  $(letE [valD (return p)
>                                                   (normalB $ [| $(envProj views (transformMOf uniplate (doToCodo) e)) gamma |]) []] [| $(codoBind bs views) $(fv "gamma") |])) |]
> codoBind ((BindS (VarP var) e):bs) views = [| $(codoBind bs ((var, Nothing) : views)) .
>                                            (coextendR (\gamma ->
>                                                       ($(envProj views (transformMOf uniplate (doToCodo) e)) gamma,
>                                                        extract gamma))) |]
> codoBind ((BindS (ParensP (ViewP viewE (VarP var))) e):bs) views = [| $(codoBind bs ((var, Just (return viewE)) : views)) .
>                                            (coextendR (\gamma ->
>                                                       ($(envProj views (transformMOf uniplate (doToCodo) e)) gamma,
>                                                        extract gamma))) |]
> codoBind ((BindS (TupP ps) e):bs) views = [| $(codoBind bs (zip (ps >>= patToVarPs) (repeat Nothing) ++ views)) .
>                                            (coextendR (\gamma ->
>                                                      $(return $ convert (concatMap patToVarPs ps) (map fst views))
>                                                       ($(envProj views (transformMOf uniplate (doToCodo) e)) gamma,
>                                                        extract gamma))) |]
> codoBind t _ = error "Ill-formed codo bindings"

> doToCodo :: Exp -> Q Exp
> doToCodo (LamE [VarP v] (DoE ((NoBindS (VarE n)):stmts)))
>              -- Nested codo-block
>              -- notably, doesn't pick up outside environment
>              | showName n == "_reserved_codo_block_marker_" = codoMain (LamE [VarP v] (DoE stmts))
>
>              | otherwise = return $ (DoE ((NoBindS (VarE n)):stmts))
> doToCodo e  = return e


> -- ***********************
> --  iii). expression transformation
> -- ***********************

> -- Creates a scope where all the local variables are projected and applied to
> -- their associated view patterns.
> envProj :: [(Name, Maybe ExpQ)]  -- ^ A mapping from names to associated view patterns.
>         -> ExpQ
>         -> ExpQ
> envProj vars exp = let gam = mkName "gamma" in (lamE [varP gam] (letE (projs vars (varE gam)) exp))

> -- Make a comonadic projection
> mkProj gam ((var,   Nothing), n) = valD                      (varP var)   (normalB [| cmapR $(prj n) $(gam) |]) []
> mkProj gam ((var, Just view), n) = valD (parensP (viewP view (varP var))) (normalB [| cmapR $(prj n) $(gam) |]) []

> -- Creates a list of projections
> projs :: [(Name,Maybe ExpQ)]  -- ^ A mapping from names to associated view patterns.
>       -> ExpQ
>       -> [DecQ]
> projs x gam =  map (mkProj gam) (zip x [0..(length x - 1)])

> -- Computes the correct projection
> prj 0 = [| fst |]
> prj n = [| $(prj (n-1)) . snd |]
