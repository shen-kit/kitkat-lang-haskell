module Parser.SemantParserTypes (module Parser.SemantParserTypes) where

import Data.List (intercalate)
import Control.Monad.Except
import Control.Monad.State (State)
import Data.Map as M hiding (map)
import Data.Text (Text, unpack)
import Parser.ParserTypes (BOp, Type)

type SExpr = (Type, SExpr')

data SExpr'
  = SInt Integer
  | SBool Bool
  | SStr Text
  | SBinOp BOp SExpr SExpr
  | SPrint SExpr SExpr -- <add '\n'?> <to_print>
  | SIdent Text
  | SCall Text [SExpr]
  deriving (Eq)

-- statements don't have an intrinsic value
data SStatement
  = SStmtExpr SExpr
  | SStmtBlock [SStatement]
  | SStmtVarDecl Type Text SExpr
  | SStmtIf SExpr SStatement SStatement -- <cond> <true_branch> <false_branch>
  | SStmtWhile SExpr SStatement -- <cond> <body>
  deriving (Eq)

-- map var_name:var_type
type Vars = M.Map Text Type

data SFunction = SFunction {styp :: Type, sname :: Text, sformals :: [Vars], sbody :: SStatement}
  deriving (Show, Eq)

-- a collection of statements and the existing bindings
data SAst = SAst {body :: [SStatement], vars :: Vars, funcs :: [SFunction]}
  deriving (Eq)

-- accumulate state to add statements and bindings
type Semant = ExceptT String (State SAst)

-- =======================================================
--                     PRETTY PRINTING                    
-- =======================================================

instance Show SAst where
  show (SAst stmts _ _) = "SAst:\n" ++ unlines (map show stmts)

instance Show SStatement where
  show (SStmtExpr expr) =
    "StmtExpr: " ++ show expr
  show (SStmtBlock stmts) =
    "StmtBlock:\n" ++ indent (unlines (map show stmts))
  show (SStmtVarDecl t name expr) =
    "StmtVarDecl: " ++ show t ++ " " ++ unpack name ++ " = " ++ show expr
  show (SStmtIf cond trueBranch falseBranch) =
    "StmtIf:\n" ++
    indent ("Cond: " ++ show cond ++ "\n") ++
    indent ("IfTrue:\n" ++ indent (show trueBranch) ++ "\n") ++
    indent ("IfFalse:\n" ++ indent (show falseBranch))
  show (SStmtWhile cond body) =
    "StmtWhile:\n" ++
    indent ("Cond: " ++ show cond ++ "\n") ++
    indent ("Body:\n" ++ indent (show body))

instance Show SExpr' where
  show (SInt i) = show i
  show (SBool b) = if b then "true" else "false"
  show (SStr s) = "\"" ++ unpack s ++ "\""
  show (SBinOp op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
  show (SPrint expr newline) =
    "print " ++ show newline ++ " " ++ show expr
  show (SIdent name) = unpack name
  show (SCall funcName args) =
    unpack funcName ++ "(" ++ intercalate ", " (map show args) ++ ")"

indent :: String -> String
indent = unlines . map ("  " ++) . lines
