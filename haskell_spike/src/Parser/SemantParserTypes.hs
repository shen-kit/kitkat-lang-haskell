module Parser.SemantParserTypes (module Parser.SemantParserTypes) where

import Control.Monad.Except
import Control.Monad.State (State)
import Data.Map as M
import Data.Text (Text)
import Parser.ParserTypes (BOp, Type)

type SExpr = (Type, SExpr')

data SExpr'
  = SInt Integer
  | SBool Bool
  | SStr Text
  | SBinOp BOp SExpr SExpr
  | SPrint SExpr SExpr -- <add '\n'?> <to_print>
  | SIdent Text
  deriving (Show, Eq)

-- statements don't have an intrinsic value
data SStatement
  = SStmtExpr SExpr
  | SStmtBlock [SStatement]
  | SStmtVarDecl Type Text SExpr
  | SStmtIf SExpr SStatement SStatement -- <cond> <true_branch> <false_branch>
  | SStmtWhile SExpr SStatement -- <cond> <body>
  deriving (Show, Eq)

-- map var_name:var_type
type Vars = M.Map Text Type

-- a collection of statements and the existing bindings
data SAst = SAst {body :: [SStatement], vars :: Vars}
  deriving (Show, Eq)

-- accumulate state to add statements and bindings
type Semant = ExceptT String (State SAst)
