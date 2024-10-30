module Parser.SemantParserTypes where

import Control.Monad.Except
import Control.Monad.State (State)
import Parser.ParserTypes (BOp, Statement, Type)

type SExpr = (Type, SExpr')

data SExpr' = SInt Int | SBinOp BOp SExpr SExpr
  deriving (Show, Eq)

data SStatement = SStmtExpr SExpr | SStmtBlock [SStatement]
  deriving (Show, Eq)

type SProgram = [SStatement]

data SemantError = TypeError {expected :: [Type], got :: Type, errorLoc :: Statement}
  deriving (Show)

type Semant = ExceptT SemantError (State SProgram)
