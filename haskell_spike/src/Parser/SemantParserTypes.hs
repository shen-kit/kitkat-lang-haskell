module Parser.SemantParserTypes where

import Data.Text (Text)
import Parser.ParserTypes (BOp, Type)

type SExpr = (Type, SExpr')

data SExpr' = SInt Int | SBinOp BOp SExpr SExpr
  deriving (Show, Eq)

data SStatement = SExpr SExpr | SBlock [SStatement]
  deriving (Show, Eq)

type SProgram = [SStatement]
