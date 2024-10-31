module Parser.SemantParserTypes where

import Control.Monad.Except
import Control.Monad.State (State)
import Parser.ParserTypes (BOp, Type)

type SExpr = (Type, SExpr')

data SExpr'
  = SInt Integer
  | SBinOp BOp SExpr SExpr
  | SPrint SExpr
  deriving (Show, Eq)

data SStatement = SStmtExpr SExpr | SStmtBlock [SStatement]
  deriving (Show, Eq)

type SProgram = [SStatement]

type Semant = ExceptT String (State SProgram)
