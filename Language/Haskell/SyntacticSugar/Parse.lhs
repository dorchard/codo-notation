> {-# LANGUAGE DeriveDataTypeable #-}

> module Language.Haskell.SyntacticSugar.Parse where

> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr
> import qualified Text.ParserCombinators.Parsec.Token as Token
> import Text.ParserCombinators.Parsec.Language
> import Language.Haskell.TH

> import Data.Generics

> type Variable = String
> type HaskExp = String

> data Binds = Bind Variable HaskExp Binds | WildBind HaskExp Binds |
>              LetBind Variable HaskExp Binds | EndExpr HaskExp
>          deriving (Show, Data, Typeable)

> data Block = Block Variable Binds
>          deriving (Show, Data, Typeable)

> parseBind :: Parser Binds
> parseBind = (try (do whiteSpace
>                      bindVar <- identifier
>                      lexeme (string "<-")
>                      exp <- tillEndOfLine
>                      remaining <- parseBind
>                      return $ Bind bindVar exp remaining
>                  ))
>                 <|>
>                 (try $ do whiteSpace
>                           lexeme (string "let")
>                           whiteSpace
>                           bindVar <- identifier
>                           whiteSpace
>                           lexeme (string "=")
>                           exp <- tillEndOfLine
>                           remaining <- parseBind
>                           return $ LetBind bindVar exp remaining)
>                 <|>
>                 (try $ do exp <- tillEndOfLine
>                           whiteSpace 
>                           eof
>                           return $ EndExpr exp)
>                 <|> 
>                 (try $ do exp <- tillEndOfLine
>                           remaining <- parseBind
>                           return $ WildBind exp remaining)
>                    
> parseBlock :: Parser Block
> parseBlock = do whiteSpace
>                 paramVar <- parens (identifier)
>                 cobinds <- parseBind
>                 return $ Block paramVar cobinds
>                
> tillEndOfLine = do { eof;
>                      return "" }
>                 <|>
>                 do { c <- anyChar;
>                      if (c=='\n') then return ""
>                        else do cs <- tillEndOfLine
>                                return (c:cs) }
>                         

> lexeme = Token.lexeme lexer
> whiteSpace = Token.whiteSpace lexer
> parens = Token.parens lexer
> identifier = Token.identifier lexer


Lex and parse routines
======================

> lexer :: Token.TokenParser ()
> lexer = haskell

> parseExpr :: Monad m => (Parser a) -> (String, Int, Int) -> String -> m a 
> parseExpr parser (file, line, col) input =
>     case (runParser p () "" input) of
>       Left err  -> fail $ show err
>       Right x   -> return x
>   where
>     p = do { pos <- getPosition;
>              setPosition $
>              (flip setSourceName) file $
>              (flip setSourceLine) line $
>              (flip setSourceColumn) col $ pos;
>              spaces;
>              x <- parser;
>              return x; }


          