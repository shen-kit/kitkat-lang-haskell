module Parser.ParserTypes (module Parser.ParserTypes) where

-- binary operators available
data BOp = Plus | Minus | Multiply | Divide
  deriving (Show, Eq, Ord)

data Expr
  = EInt Integer
  | EBinOp BOp Expr Expr
  | EPrint Expr
  deriving (Show, Eq, Ord)

-- a statement is one or more expressions
data Statement = StmtExpr Expr | StmtBlock [Statement]
  deriving (Show, Eq, Ord)

-- types that an expression can return (e.g. addition of integers has type int)
data Type = TyInt | TyVoid
  deriving (Show, Eq, Ord)

-- root of the AST
newtype Ast = Ast [Statement]
  deriving (Show, Eq)
