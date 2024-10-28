module Lexer.TokenTypes where

import Data.Text (Text)

data Token
  = TInt Int
  | TBinOp Text
  deriving (Show, Eq, Ord)
