{-# LANGUAGE OverloadedStrings #-}
module Main where

import TestUtils
import Data.Either
import Test.HUnit
import Text.Megaparsec
import Lexer.Inner
import Lexer.TokenTypes

-- ============================================================
-- =                    INDIVIDUAL PARSERS                    = 
-- ============================================================

-- test primitive parsers
tPrimitives :: Test
tPrimitives        = TestList [tInt, tIntZero, tIntZeroPrefix, tNegativeInt, tStr, tStrEmpty, tStrWithRWord]
  where
    tInt           = testParseEq pInt "123" $ TInt 123
    tIntZero       = testParseEq pInt "0" $ TInt 0
    tIntZeroPrefix = testParseEq pInt "00123" $ TInt 123
    tNegativeInt   = testParseFail pInt "-1"
    tStr           = testParseEq pStr "\"test\"" $ TString "test"
    tStrEmpty      = testParseEq pStr "\"\"" $ TString ""
    tStrWithRWord  = testParseEq pStr "\"true\"" $ TString "true"

-- test reserved words handled correctly
tRWord :: Test
tRWord       = TestList [tTrue, tFalse]
  where
    tInt     = testParseEq pRWords "int" $ TRWord "int"
    tBool    = testParseEq pRWords "bool" $ TRWord "bool"
    tStr     = testParseEq pRWords "str" $ TRWord "str"
    tTrue    = testParseEq pRWords "true" $ TRWord "true"
    tFalse   = testParseEq pRWords "false" $ TRWord "false"
    tNull    = testParseEq pRWords "null" $ TRWord "null"
    tIf      = testParseEq pRWords "if" $ TRWord "if"
    tElse    = testParseEq pRWords "else" $ TRWord "else"
    tWhile   = testParseEq pRWords "while" $ TRWord "while"
    tPrint   = testParseEq pRWords "print" $ TRWord "print"
    tPrintln = testParseEq pRWords "println" $ TRWord "println"

-- test testParseEq pRWords parsing binary operators
tBinOp :: Test
tBinOp        = TestList [tGe, tLe, tGt, tLt, tEq, tNeq, tAdd, tSubtract, tMultiply, tDivide, tAssign, tAnd, tOr]
  where
    tGe       = testParseEq pBinOp  ">=" $ TBinOp ">="
    tLe       = testParseEq pBinOp  "<=" $ TBinOp "<="
    tGt       = testParseEq pBinOp  ">" $ TBinOp ">"
    tLt       = testParseEq pBinOp  "<" $ TBinOp "<"
    tEq       = testParseEq pBinOp  "==" $ TBinOp "=="
    tNeq      = testParseEq pBinOp  "!=" $ TBinOp "!="
    tAdd      = testParseEq pBinOp  "+" $ TBinOp "+"
    tSubtract = testParseEq pBinOp  "-" $ TBinOp "-"
    tMultiply = testParseEq pBinOp  "*" $ TBinOp "*"
    tDivide   = testParseEq pBinOp  "/" $ TBinOp "/"
    tAssign   = testParseEq pBinOp  "=" $ TBinOp "="
    tAnd      = testParseEq pBinOp  "&" $ TBinOp "&"
    tOr       = testParseEq pBinOp  "|" $ TBinOp "|"

-- test parsing symbols
tSymbol :: Test
tSymbol = TestList [tSemi, tLParen, tRParen, tLBrace, tRBrace]
  where
    tSemi   = testParseEq pSymbol ";" $ TSemi
    tLParen = testParseEq pSymbol "(" $ TLParen
    tRParen = testParseEq pSymbol ")" $ TRParen
    tLBrace = testParseEq pSymbol "{" $ TLBrace
    tRBrace = testParseEq pSymbol "}" $ TRBrace

-- test parsing identifiers
tIdent :: Test
tIdent = TestList [tIdentLower, tIdentUpper, tIdentDigitPrefix, tIdentDigit]
 where
  tIdentLower       = testParseEq pIdent "myidentifier" $ TIdent "myidentifier"
  tIdentUpper       = testParseEq pIdent "MYIDENTIFIER" $ TIdent "MYIDENTIFIER"
  tIdentDigitPrefix = testParseFail pIdent "1invalid"
  tIdentDigit       = testParseEq pIdent "valid123Identifier" $ TIdent "valid123Identifier"

-- ===========================================================
-- =                       FULL PARSER                       = 
-- ===========================================================

-- test the lexer function (chaining lexers)
tLexer :: Test
tLexer = TestList [tEmptyInput, tComments, tLexerSingle, tLexerMany]
  where
    tEmptyInput = testParseEq lexer "" $ [TEOF]
    tComments = testParseEq lexer "# comment" $ [TEOF]
    tLexerSingle = testParseEq lexer "1" $ [TInt 1, TEOF]
    -- lex a long string containing all types of tokens
    tLexerMany = testParseEq lexer  "(int 123) & {str \"mystring\"} | bool true + myIdentifier / null; # keywords\nif else == while != print >= println " $ [TLParen,TRWord "int",TInt 123,TRParen,TBinOp "&",TLBrace,TRWord "str",TString "mystring",TRBrace,TBinOp "|",TRWord "bool",TRWord "true",TBinOp "+",TIdent "myIdentifier",TBinOp "/",TRWord "null",TSemi,TRWord "if",TRWord "else",TBinOp "==",TRWord "while",TBinOp "!=",TRWord "print",TBinOp ">=",TRWord "println",TEOF]

tests :: Test
tests = TestList [tPrimitives, tRWord, tBinOp, tSymbol, tIdent, tLexer]

main :: IO ()
main = runTestTTAndExit tests
