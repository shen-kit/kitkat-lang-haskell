module Parser.ParserTypes (module Parser.ParserTypes) where

-- binary operators available
data BOp = Plus | Minus | Multiply | Divide | Modulus
  deriving (Show, Eq, Ord)

data Expr
  = EInt Int
  | EBinOp BOp Expr Expr
  deriving (Show, Eq, Ord)

-- a statement is one or more expressions
data Statement = StmtExpr Expr | StmtBlock [Statement]
  deriving (Show, Eq, Ord)

-- types that an expression can return (e.g. addition of integers has type int)
data Type = TyInt
  deriving (Show, Eq, Ord)

-- a program is a list of bindings
newtype Program = Program [Statement]
  deriving (Show, Eq)
