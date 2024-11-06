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
    tInt           = testEq pInt "123" $ TInt 123
    tIntZero       = testEq pInt "0" $ TInt 0
    tIntZeroPrefix = testEq pInt "00123" $ TInt 123
    tNegativeInt   = testFail pInt "-1"
    tStr           = testEq pStr "\"test\"" $ TString "test"
    tStrEmpty      = testEq pStr "\"\"" $ TString ""
    tStrWithRWord  = testEq pStr "\"true\"" $ TString "true"

-- test reserved words handled correctly
tRWord :: Test
tRWord       = TestList [tTrue, tFalse]
  where
    tInt     = testEq pRWords "int" $ TRWord "int"
    tBool    = testEq pRWords "bool" $ TRWord "bool"
    tStr     = testEq pRWords "str" $ TRWord "str"
    tTrue    = testEq pRWords "true" $ TRWord "true"
    tFalse   = testEq pRWords "false" $ TRWord "false"
    tNull    = testEq pRWords "null" $ TRWord "null"
    tIf      = testEq pRWords "if" $ TRWord "if"
    tElse    = testEq pRWords "else" $ TRWord "else"
    tWhile   = testEq pRWords "while" $ TRWord "while"
    tPrint   = testEq pRWords "print" $ TRWord "print"
    tPrintln = testEq pRWords "println" $ TRWord "println"

-- test testEq pRWords parsing binary operators
tBinOp :: Test
tBinOp        = TestList [tGe, tLe, tGt, tLt, tEq, tNeq, tAdd, tSubtract, tMultiply, tDivide, tAssign, tAnd, tOr]
  where
    tGe       = testEq pBinOp  ">=" $ TBinOp ">="
    tLe       = testEq pBinOp  "<=" $ TBinOp "<="
    tGt       = testEq pBinOp  ">" $ TBinOp ">"
    tLt       = testEq pBinOp  "<" $ TBinOp "<"
    tEq       = testEq pBinOp  "==" $ TBinOp "=="
    tNeq      = testEq pBinOp  "!=" $ TBinOp "!="
    tAdd      = testEq pBinOp  "+" $ TBinOp "+"
    tSubtract = testEq pBinOp  "-" $ TBinOp "-"
    tMultiply = testEq pBinOp  "*" $ TBinOp "*"
    tDivide   = testEq pBinOp  "/" $ TBinOp "/"
    tAssign   = testEq pBinOp  "=" $ TBinOp "="
    tAnd      = testEq pBinOp  "&" $ TBinOp "&"
    tOr       = testEq pBinOp  "|" $ TBinOp "|"

-- test parsing symbols
tSymbol :: Test
tSymbol = TestList [tSemi, tLParen, tRParen, tLBrace, tRBrace]
  where
    tSemi   = testEq pSymbol ";" $ TSemi
    tLParen = testEq pSymbol "(" $ TLParen
    tRParen = testEq pSymbol ")" $ TRParen
    tLBrace = testEq pSymbol "{" $ TLBrace
    tRBrace = testEq pSymbol "}" $ TRBrace

tIdent :: Test
tIdent = TestList [tIdentLower, tIdentUpper, tIdentDigitPrefix, tIdentDigit]
 where
  tIdentLower       = testEq pIdent "myidentifier" $ TIdent "myidentifier"
  tIdentUpper       = testEq pIdent "MYIDENTIFIER" $ TIdent "MYIDENTIFIER"
  tIdentDigitPrefix = testFail pIdent "1invalid"
  tIdentDigit       = testEq pIdent "valid123Identifier" $ TIdent "valid123Identifier"

-- ============================================================
-- =                    INDIVIDUAL PARSERS                    = 
-- ============================================================

-- test the lexer function (chaining lexers)
tLexer :: Test
tLexer = TestList [tEmptyInput, tComments, tLexerSingle, tLexerMany]
  where
    tEmptyInput = testEq lexer "" $ [TEOF]
    tComments = testEq lexer "# comment" $ [TEOF]
    tLexerSingle = testEq lexer "1" $ [TInt 1, TEOF]
    -- lex a long string containing all types of tokens
    tLexerMany = testEq lexer  "(int 123) & {str \"mystring\"} | bool true + myIdentifier / null; # keywords\nif else == while != print >= println " $ [TLParen,TRWord "int",TInt 123,TRParen,TBinOp "&",TLBrace,TRWord "str",TString "mystring",TRBrace,TBinOp "|",TRWord "bool",TRWord "true",TBinOp "+",TIdent "myIdentifier",TBinOp "/",TRWord "null",TSemi,TRWord "if",TRWord "else",TBinOp "==",TRWord "while",TBinOp "!=",TRWord "print",TBinOp ">=",TRWord "println",TEOF]

tests :: Test
tests = TestList [tPrimitives, tRWord, tBinOp, tSymbol, tIdent, tLexer]

main :: IO ()
main = runTestTTAndExit tests
