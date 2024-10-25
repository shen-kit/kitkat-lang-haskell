module Types (Expr (..)) where

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
