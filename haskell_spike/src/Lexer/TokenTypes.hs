module Lexer.TokenTypes where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, satisfy)

data Token
  = TInt Integer
  | TString Text
  | TBinOp Text
  | TRWord Text
  | TIdent String -- annoying to convert all parsers to text, convert later
  | TLParen -- (
  | TRParen -- )
  | TLBrace -- {
  | TRBrace -- }
  | TSQuote -- '
  | TDQuote -- "
  | TSemi -- ;
  | TEOF
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

isIdent :: Token -> Bool
isIdent (TIdent _) = True
isIdent _ = False

isBool :: Token -> Bool
isBool (TRWord "true") = True
isBool (TRWord "false") = True
isBool _ = False

isStr :: Token -> Bool
isStr (TString _) = True
isStr _ = False
