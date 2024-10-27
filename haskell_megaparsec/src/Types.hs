module Types (Token (..)) where

import Data.Text (Text)

-- lexer token types

data Token
  = -- literals
    TInt Int
  | TFloat Float
  | TStringLit Text
  | TCharLit Char
  | TBool Bool
  | TNull
  | -- keywords / identifiers
    TIdent String -- var/func names
  | TKeyword Text
  | -- operators
    TUnOp Text -- e.g. '-', '++'
  | TBinOp Char
  | TEquals
  | -- flow control
    TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | -- delimiters / symbols
    TSemicolon
  | TColon
  | TComma
  deriving (Eq, Ord, Show)

-- AST type definitions

data Expr
  = IDENT String -- identifier name
  | INT Int -- -1, 0, 1, ...
  | FLOAT Float -- 1.0, 3.14
  | NEG Expr -- -(expr)
  | SUM Expr Expr -- (expr) + (expr)
  | DIFFERENCE Expr Expr -- (expr) - (expr)
  | PRODUCT Expr Expr -- (expr) * (expr)
  | DIVISION Expr Expr -- (expr) / (expr)
  | MODULUS Expr Expr -- (expr) % (expr)
  deriving (Eq, Ord, Show)
