{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parseProgram) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.String (IsString (fromString))
import Data.Void (Void)
import Lexer.TokenTypes
import Parser.ParserTypes (Ast (Ast), BOp (..), Expr (..), Statement (..), Type (..))
import Text.Megaparsec (Parsec, between, choice, many, satisfy)

-- parser for a list of tokens
type TokParser = Parsec Void [Token]

-- ==================== PARSERS ====================

-- end with EOF token -> fails if can't parse tokens into an AST
parseProgram :: TokParser Ast
parseProgram = Ast <$> many parseStmt <* isTok TEOF

parseStmt :: TokParser Statement
parseStmt = pStmt <* isTok TSemi
  where
    pStmt :: TokParser Statement
    pStmt =
      pVarDecl
        <|> StmtExpr <$> (pPrint <|> pExpr)

opTable :: [[Operator TokParser Expr]]
opTable =
  [ [binL Multiply "*", binL Divide "/"],
    [binL Plus "+", binL Minus "-"],
    [binL LAnd "&", binL LOr "|"],
    [binR Assign "="]
  ]
  where
    binL opType opText = InfixL $ EBinOp opType <$ isTok (TBinOp opText)
    binR opType opText = InfixR $ EBinOp opType <$ isTok (TBinOp opText)

pTerm :: TokParser Expr
pTerm = choice [parseInt, parseIdent, parseBrackets, parseBool]
  where
    parseInt = do
      TInt val <- satisfy isInt
      pure $ EInt val
    parseIdent = do
      TIdent vname <- satisfy isIdent
      pure $ EIdent $ fromString vname
    parseBrackets = between (isTok TLParen) (isTok TRParen) pExpr
    parseBool = do
      TRWord word <- satisfy isBool
      pure $ EBool $ word == "true"

-- TODO: desugar to be the same as any other function
pPrint :: TokParser Expr
pPrint = do
  _ <- isTok (TRWord "print")
  _ <- isTok TLParen
  e <- pExpr
  _ <- isTok TRParen
  pure $ EPrint e

pExpr :: TokParser Expr
pExpr = makeExprParser pTerm opTable

-- parse bindings

pType :: TokParser Type
pType =
  choice
    [ TyInt <$ isTok (TRWord "int"),
      TyNull <$ isTok (TRWord "null"),
      TyBool <$ isTok (TRWord "bool")
    ]

pIdent :: TokParser Expr
pIdent = do
  TIdent name <- satisfy isIdent
  pure $ EIdent $ fromString name

pVarDecl :: TokParser Statement
pVarDecl = do
  vType <- pType
  (EIdent ident) <- pIdent
  value <- isTok (TBinOp "=") *> pExpr
  pure $ StmtVarDecl vType ident value
