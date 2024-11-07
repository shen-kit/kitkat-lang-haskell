module Parser.ParserTypes (module Parser.ParserTypes) where

import Data.List (intercalate)
import Data.Text (Text, unpack)

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
  | ECall Text [Expr] -- function call, <func_name> [<args>]
  deriving (Eq, Ord)

-- define separate name for clarity in constructors
type VarName = Expr

-- a statement is one or more expressions
data Statement
  = StmtExpr Expr
  | StmtBlock [Statement]
  | StmtVarDecl Type Text Expr
  | StmtIf Expr Statement Statement -- <condition> <true_branch> <false_branch>
  | StmtWhile Expr Statement -- <condition> <loop_body>
  deriving (Eq, Ord)

-- types that an expression can return (e.g. addition of integers has type int)
data Type
  = TyInt
  | TyBool
  | TyStr
  | TyNull
  deriving (Show, Eq, Ord)

-- root of the AST -> contains declarations and statements
newtype Ast = Ast [Statement]
  deriving (Eq)

-- =======================================================
--                     PRETTY PRINTING                    
-- =======================================================

-- indent nested statements
indent :: String -> String
indent = unlines . map ("  " ++) . lines

instance Show Ast where
  show (Ast stmts) = "AST:\n" ++ unlines (show <$> stmts)

instance Show Statement where
  show (StmtExpr expr) =
    "StmtExpr: " ++ show expr
  show (StmtBlock stmts) =
    "StmtBlock:\n" ++ indent (unlines (map show stmts))
  show (StmtVarDecl t name expr) =
    "StmtVarDecl: " ++ show t ++ " " ++ unpack name ++ " = " ++ show expr
  show (StmtIf cond trueBranch falseBranch) =
    "StmtIf:\n" ++
    indent ("Cond: " ++ show cond ++ "\n") ++
    indent ("IfTrue:\n" ++ indent (show trueBranch) ++ "\n") ++
    indent ("IfFalse:\n" ++ indent (show falseBranch))
  show (StmtWhile cond body) =
    "StmtWhile:\n" ++
    indent ("Cond: " ++ show cond ++ "\n") ++
    indent ("Body:\n" ++ indent (show body))

instance Show Expr where
  show (EInt i) = show i
  show (EBool b) = if b then "true" else "false"
  show (EString s) = "\"" ++ unpack s ++ "\""
  show (EBinOp op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
  show (EPrint newline expr) =
    "print " ++ show newline ++ " " ++ show expr
  show (EIdent name) = unpack name
  show (ECall funcName args) =
    (unpack funcName) ++ "(" ++ intercalate ", " (map show args) ++ ")"
