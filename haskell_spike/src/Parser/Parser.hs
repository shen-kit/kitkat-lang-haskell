{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (pProgram) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Void (Void)
import Lexer.TokenTypes
import Parser.ParserTypes (Ast (Ast), BOp (..), Expr (..), Statement (StmtExpr))
import Text.Megaparsec (Parsec, many, satisfy)

-- parser for a list of tokens
type ParserT = Parsec Void [Token]

-- ==================== PARSERS ====================

parseInt :: ParserT Expr
parseInt = do
  TInt val <- satisfy isInt
  return $ EInt val

opTable :: [[Operator ParserT Expr]]
opTable =
  [ [binL Multiply "*", binL Divide "/"],
    [binL Plus "+", binL Minus "-"]
  ]
  where
    binL opType opText = InfixL $ EBinOp opType <$ isTok (TBinOp opText)

pTerm :: ParserT Expr
pTerm = parseInt

pPrint :: ParserT Expr
pPrint = do
  isTok (TKeyword "print")
  isTok TLParen
  e <- pExpr
  isTok TRParen
  pure $ EPrint e

pExpr :: ParserT Expr
pExpr = makeExprParser pTerm opTable

pStmt :: ParserT Statement
pStmt = StmtExpr <$> (pPrint <|> pExpr) <* isTok TSemi

pProgram :: ParserT Ast
pProgram = Ast <$> many pStmt
