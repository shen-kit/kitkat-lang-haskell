{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Parser.Parser (parseProgram) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.String (IsString (fromString))
import Data.Void (Void)
import Lexer.TokenTypes
import Parser.ParserTypes (Ast (Ast), BOp (..), Expr (..), Statement (..), Type (..))
import Text.Megaparsec (Parsec, between, choice, many, option, satisfy)

-- ==================== TYPES + HELPERS ====================

-- parser for a list of tokens
type TokParser = Parsec Void [Token]

-- parse a semicolon
semi :: TokParser Token
semi = isTok TSemi

-- ==================== PARSERS ====================

-- end with EOF token -> fails if can't parse tokens into an AST
parseProgram :: TokParser Ast
parseProgram = Ast <$> many parseStmt <* isTok TEOF

parseStmt :: TokParser Statement
parseStmt =
  choice
    [ pExprStmt,
      pVarDecl,
      pBlock,
      pIf,
      pWhile
    ]
  where
    -- expression, terminated by semicolon
    pExprStmt :: TokParser Statement
    pExprStmt = StmtExpr <$> (pPrint <|> pPrintln <|> pExpr) <* semi

    -- <type> <var_name> = <expr>;
    pVarDecl = do
      vType <- pType
      (EIdent ident) <- pIdent
      value <- isTok (TBinOp "=") *> pExpr <* semi
      pure $ StmtVarDecl vType ident value

    -- { [stmts] }
    pBlock = StmtBlock <$> between (isTok TLBrace) (isTok TRBrace) (many parseStmt)

    -- if <expr> <stmt>
    pIf = mdo
      _ <- isTok (TRWord "if")
      cond <- pExpr
      body <- parseStmt
      maybeElse <- option (StmtBlock []) (isTok (TRWord "else") *> parseStmt)
      pure $ StmtIf cond body maybeElse

    -- if <expr> <stmt>
    pWhile = mdo
      _ <- isTok (TRWord "while")
      cond <- pExpr
      body <- parseStmt
      pure $ StmtWhile cond body

opTable :: [[Operator TokParser Expr]]
opTable =
  [ [binL Multiply "*", binL Divide "/"],
    [binL Plus "+", binL Minus "-"],
    [binL Gt ">", binL Lt "<", binL Ge ">=", binL Le "<=", binL Eq "==", binL NEq "!="],
    [binL LAnd "&", binL LOr "|"],
    [binR Assign "="]
  ]
  where
    binL opType sym = InfixL $ EBinOp opType <$ isTok (TBinOp sym)
    -- for operators that are prefixes of other operators
    -- binL' opType sym = InfixL $ EBinOp opType <$ operator sym
    binR opType sym = InfixR $ EBinOp opType <$ isTok (TBinOp sym)

pTerm :: TokParser Expr
pTerm = choice [parseInt, parseIdent, parseBrackets, parseBool, parseStr]
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
    parseStr = do
      TString s <- satisfy isStr
      pure $ EString s

-- TODO: desugar to be the same as any other function
pPrint :: TokParser Expr
pPrint = do
  _ <- isTok (TRWord "print")
  _ <- isTok TLParen
  e <- pExpr
  _ <- isTok TRParen
  pure $ EPrint False e

pPrintln :: TokParser Expr
pPrintln = do
  _ <- isTok (TRWord "println")
  _ <- isTok TLParen
  e <- pExpr
  _ <- isTok TRParen
  pure $ EPrint True e

pExpr :: TokParser Expr
pExpr = makeExprParser pTerm opTable

pType :: TokParser Type
pType =
  choice
    [ TyInt <$ isTok (TRWord "int"),
      TyNull <$ isTok (TRWord "null"),
      TyBool <$ isTok (TRWord "bool"),
      TyStr <$ isTok (TRWord "str")
    ]

pIdent :: TokParser Expr
pIdent = do
  TIdent name <- satisfy isIdent
  pure $ EIdent $ fromString name
