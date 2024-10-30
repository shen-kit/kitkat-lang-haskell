module Lexer.TokenTypes where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, satisfy)

data Token
  = TInt Int
  | TBinOp Text
  | Semi
  deriving (Show, Eq, Ord)

-- helper type checks

-- returns a parser that matches a specific token (exactly)
-- if the input is the same token, return the token, else fail
isTok :: Token -> Parsec Void [Token] Token
isTok t = satisfy (== t)

isInt :: Token -> Bool
isInt (TInt {}) = True
isInt _ = False

isBOp :: Token -> Bool
isBOp (TBinOp {}) = True
isBOp _ = False
