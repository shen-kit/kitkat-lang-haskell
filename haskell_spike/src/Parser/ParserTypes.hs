module Parser.ParserTypes (module Parser.ParserTypes) where

import Data.Text (Text)

-- binary operators available
data BOp
  = Plus
  | Minus
  | Multiply
  | Divide
  | Assign -- equals sign
  | LAnd -- logical AND
  | LOr -- logical OR
  | Gt
  | Lt
  | Ge
  | Le
  | Eq
  | NEq
  deriving (Show, Eq, Ord)

data Expr
  = EInt Integer
  | EBool Bool
  | EString Text
  | EBinOp BOp Expr Expr
  | EPrint Expr Expr -- <add '\n'?> <to_print>
  | EIdent Text
  deriving (Show, Eq, Ord)

-- define separate name for clarity in constructors
type VarName = Expr

-- a statement is one or more expressions
data Statement
  = StmtExpr Expr
  | StmtBlock [Statement]
  | StmtVarDecl Type Text Expr
  | StmtIf Expr Statement Statement -- <condition> <true_branch> <false_branch>
  | StmtWhile Expr Statement -- <condition> <loop_body>
  deriving (Show, Eq, Ord)

-- types that an expression can return (e.g. addition of integers has type int)
data Type
  = TyInt
  | TyBool
  | TyStr
  | TyNull
  deriving (Show, Eq, Ord)

-- root of the AST -> contains declarations and statements
newtype Ast = Ast [Statement]
  deriving (Show, Eq)
