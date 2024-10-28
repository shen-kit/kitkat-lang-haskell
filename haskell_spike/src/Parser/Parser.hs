{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Void (Void)
import Lexer.TokenTypes (Token (TBinOp, TInt))
import Parser.ParserTypes
import Text.Megaparsec (Parsec, many, satisfy)

-- parser for a list of tokens
type ParserT = Parsec Void [Token]

-- ==================== HELPERS ====================

-- returns a parser that matches a specific token
-- if the input is the same token, return the token, else fail
isTok :: Token -> ParserT Token
isTok t = satisfy (== t)

-- ==================== PARSERS ====================

parseInt :: ParserT Expr
parseInt = do
  TInt val <- satisfy isInt
  return $ EInt val
  where
    isInt (TInt _) = True
    isInt _ = False

opTable :: [[Operator ParserT Expr]]
opTable =
  [ [binL Multiply "*", binL Divide "/", binL Modulus "%"],
    [binL Plus "+", binL Minus "-"]
  ]
  where
    binL opType opText = InfixL $ BinOp opType <$ isTok (TBinOp opText)

pTerm :: ParserT Expr
pTerm = parseInt

pExpr :: ParserT Expr
pExpr = makeExprParser pTerm opTable

pProgram :: ParserT Program
pProgram = Program <$> many pExpr
