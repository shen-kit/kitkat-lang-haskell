{-# LANGUAGE OverloadedStrings #-}

module Parser (parseTokens) where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Combinators.Expr
import Data.Either
import Data.Text (Text)
import Data.Void (Void)
import Lexer.Lexer (symbol)
import Text.Megaparsec hiding (Token)
import Types (Expr (..), Token (..))

type Parser = Parsec Void [Token]

isTok :: Token -> Parser Token
isTok t = satisfy (== t)

pInt :: Parser Expr
pInt = do
  TInt val <- satisfy isIntTok
  pure $ IntLit val
  where
    isIntTok (TInt _) = True
    isIntTok _ = False

pParens :: Parser Expr -> Parser Expr
pParens = between (isTok TLParen) (isTok TRParen)

opTable :: [[Operator Parser Expr]]
opTable =
  [ [infixL "*", infixL "/", infixL "%"],
    [infixL "+", infixL "-"],
    [infixL ">=", infixL ">", infixL "<=", infixL "<", infixL "==", infixL "!="],
    [infixL "+", infixL "-"]
  ]
  where
    infixL :: Text -> Operator Parser Expr
    infixL match = InfixL $ BinOp match <$ isTok (TBinOp match)

pTerm :: Parser Expr
pTerm = pParens pExpr <|> pInt

pExpr :: Parser Expr
pExpr = makeExprParser pTerm opTable

parseTokens :: [Token] -> Either (ParseErrorBundle [Token] Void) Expr
parseTokens = runParser (pExpr <* eof) ""
