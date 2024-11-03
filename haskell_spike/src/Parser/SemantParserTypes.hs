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
  | SBinOp BOp SExpr SExpr
  | SPrint SExpr
  | SIdent Text
  deriving (Show, Eq)

-- statements don't have an intrinsic value
data SStatement
  = SStmtExpr SExpr
  | SStmtBlock [SStatement]
  | SStmtVarDecl Type Text SExpr
  deriving (Show, Eq)

type SAst = [SStatement]

type Semant = ExceptT String (State SAst)
