module Parser.ParserTypes (module Parser.ParserTypes) where

import Data.Text (Text)

-- binary operators available
data BOp
  = Plus
  | Minus
  | Multiply
  | Divide
  | Eq -- var declaration + assignment
  deriving (Show, Eq, Ord)

data Expr
  = EInt Integer
  | EBinOp BOp Expr Expr
  | EPrint Expr
  | EAssign VarName Expr -- assign variable
  | EIdent Text
  deriving (Show, Eq, Ord)

-- define separate name for clarity in constructors
type VarName = Expr

-- a statement is one or more expressions
data Statement
  = StmtExpr Expr
  | StmtBlock [Statement]
  | StmtVarDecl Type Text Expr
  deriving (Show, Eq, Ord)

-- types that an expression can return (e.g. addition of integers has type int)
data Type
  = TyInt
  | TyNull
  deriving (Show, Eq, Ord)

-- root of the AST -> contains declarations and statements
newtype Ast = Ast [Statement]
  deriving (Show, Eq)
