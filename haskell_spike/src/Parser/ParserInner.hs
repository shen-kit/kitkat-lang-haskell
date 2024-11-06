{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Parser.ParserInner where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.String (IsString (fromString))
import Data.Void (Void)
import Lexer.TokenTypes
import Parser.ParserTypes (Ast (Ast), BOp (..), Expr (..), Statement (..), Type (..))
import Text.Megaparsec (MonadParsec (try), Parsec, between, choice, many, option, satisfy, sepBy)

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
    pExprStmt = StmtExpr <$> pExpr <* semi

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

pExpr :: TokParser Expr
pExpr = choice [makeExprParser pTerm opTable, pPrint, pIdent]
  where
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

    pParens :: TokParser a -> TokParser a
    pParens = between (isTok TLParen) (isTok TRParen)

    pTerm :: TokParser Expr
    pTerm = choice [parseInt, pParens pExpr, parseBool, parseStr, pFunc, pIdent]
      where
        parseInt = do
          TInt val <- satisfy isInt
          pure $ EInt val
        parseBool = do
          TRWord word <- satisfy isBool
          pure $ EBool $ word == "true"
        parseStr = do
          TString s <- satisfy isStr
          pure $ EString s
        pFunc = try $ do
          (EIdent ident) <- pIdent
          args <- pParens (pExpr `sepBy` isTok TComma)
          pure $ ECall ident args

    pPrint :: TokParser Expr
    pPrint = do
      _ <- isTok (TRWord "print")
      _ <- isTok TLParen
      e <- pExpr
      maybeEnd <- option (EBool True) $ isTok TComma *> pExpr
      _ <- isTok TRParen
      pure $ EPrint maybeEnd e

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
