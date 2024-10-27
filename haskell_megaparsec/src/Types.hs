module Types (module Types) where

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
  | TBinOp Text
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
  = Ident String -- identifier name
  | IntLit Int -- -1, 0, 1, ...
  | FloatLit Float -- 1.0, 3.14
  | StringLit Text -- 1.0, 3.14
  | CharLit Char -- 1.0, 3.14
  | UnOp Text Expr -- -(expr)
  | BinOp Text Expr Expr -- (expr) + (expr)
  deriving (Eq, Ord, Show)
