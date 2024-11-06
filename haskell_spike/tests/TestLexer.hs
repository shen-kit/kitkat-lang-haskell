{-# LANGUAGE OverloadedStrings #-}
module Main where

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
    tInt           = TestCase (assertEqual "pInt normal input" (Right $ TInt 123) (runParser pInt "" "123"))
    tIntZero       = TestCase (assertEqual "pInt zero" (Right $ TInt 0) (runParser pInt "" "0"))
    tIntZeroPrefix = TestCase (assertEqual "pInt left-padded input" (Right $ TInt 123) (runParser pInt "" "00123"))
    tNegativeInt   = TestCase (assertBool "pInt negative, should fail (unary operator)" (isLeft $ runParser pInt "" "-1"))
    tStr           = TestCase (assertEqual "pStr normal input" (Right $ TString "test string") (runParser pStr "" "\"test string\""))
    tStrEmpty      = TestCase (assertEqual "pStr empty string" (Right $ TString "") (runParser pStr "" "\"\""))
    tStrWithRWord  = TestCase (assertEqual "pStr keyword in quotes" (Right $ TString "true") (runParser pStr "" "\"true\""))

-- test reserved words handled correctly
tRWord :: Test
tRWord       = TestList [tTrue, tFalse]
  where
    tInt     = TestCase (assertEqual "test reserved word: int" (Right $ TRWord "int") (runParser pRWords "" "int"))
    tBool    = TestCase (assertEqual "test reserved word: bool" (Right $ TRWord "bool") (runParser pRWords "" "bool"))
    tStr     = TestCase (assertEqual "test reserved word: str" (Right $ TRWord "str") (runParser pRWords "" "str"))
    tTrue    = TestCase (assertEqual "test reserved word: true" (Right $ TRWord "true") (runParser pRWords "" "true"))
    tFalse   = TestCase (assertEqual "test reserved word: false" (Right $ TRWord "false") (runParser pRWords "" "false"))
    tNull    = TestCase (assertEqual "test reserved word: null" (Right $ TRWord "null") (runParser pRWords "" "null"))
    tIf      = TestCase (assertEqual "test reserved word: if" (Right $ TRWord "if") (runParser pRWords "" "if"))
    tElse    = TestCase (assertEqual "test reserved word: else" (Right $ TRWord "else") (runParser pRWords "" "else"))
    tWhile   = TestCase (assertEqual "test reserved word: while" (Right $ TRWord "while") (runParser pRWords "" "while"))
    tPrint   = TestCase (assertEqual "test reserved word: print" (Right $ TRWord "print") (runParser pRWords "" "print"))
    tPrintln = TestCase (assertEqual "test reserved word: println" (Right $ TRWord "println") (runParser pRWords "" "println"))

-- test parsing binary operators
tBinOp :: Test
tBinOp        = TestList [tGe, tLe, tGt, tLt, tEq, tNeq, tAdd, tSubtract, tMultiply, tDivide, tAssign, tAnd, tOr]
  where
    tGe       = TestCase (assertEqual "test binary operator: >=" (Right $ TBinOp ">=") (runParser pBinOp "" ">="))
    tLe       = TestCase (assertEqual "test binary operator: <=" (Right $ TBinOp "<=") (runParser pBinOp "" "<="))
    tGt       = TestCase (assertEqual "test binary operator: >" (Right $ TBinOp ">") (runParser pBinOp "" ">"))
    tLt       = TestCase (assertEqual "test binary operator: <" (Right $ TBinOp "<") (runParser pBinOp "" "<"))
    tEq       = TestCase (assertEqual "test binary operator: ==" (Right $ TBinOp "==") (runParser pBinOp "" "=="))
    tNeq      = TestCase (assertEqual "test binary operator: !=" (Right $ TBinOp "!=") (runParser pBinOp "" "!="))
    tAdd      = TestCase (assertEqual "test binary operator: +" (Right $ TBinOp "+") (runParser pBinOp "" "+"))
    tSubtract = TestCase (assertEqual "test binary operator: -" (Right $ TBinOp "-") (runParser pBinOp "" "-"))
    tMultiply = TestCase (assertEqual "test binary operator: *" (Right $ TBinOp "*") (runParser pBinOp "" "*"))
    tDivide   = TestCase (assertEqual "test binary operator: /" (Right $ TBinOp "/") (runParser pBinOp "" "/"))
    tAssign   = TestCase (assertEqual "test binary operator: =" (Right $ TBinOp "=") (runParser pBinOp "" "="))
    tAnd      = TestCase (assertEqual "test binary operator: &" (Right $ TBinOp "&") (runParser pBinOp "" "&"))
    tOr       = TestCase (assertEqual "test binary operator: |" (Right $ TBinOp "|") (runParser pBinOp "" "|"))

-- test parsing symbols
tSymbol :: Test
tSymbol = TestList [tSemi, tLParen, tRParen, tLBrace, tRBrace]
  where
    tSemi   = TestCase (assertEqual "test symbol: ;" (Right TSemi) (runParser pSymbol "" ";"))
    tLParen = TestCase (assertEqual "test symbol: (" (Right TLParen) (runParser pSymbol "" "("))
    tRParen = TestCase (assertEqual "test symbol: )" (Right TRParen) (runParser pSymbol "" ")"))
    tLBrace = TestCase (assertEqual "test symbol: {" (Right TLBrace) (runParser pSymbol "" "{"))
    tRBrace = TestCase (assertEqual "test symbol: }" (Right TRBrace) (runParser pSymbol "" "}"))

tIdent :: Test
tIdent = TestList [tIdentLower, tIdentUpper, tIdentDigitPrefix, tIdentDigit]
 where
  tIdentLower       = TestCase (assertEqual "test identifier lowercase" (Right $ TIdent "myidentifier") (runParser pIdent "" "myidentifier"))
  tIdentUpper       = TestCase (assertEqual "test identifier uppercase" (Right $ TIdent "MYIDENTIFIER") (runParser pIdent "" "MYIDENTIFIER"))
  tIdentDigitPrefix = TestCase (assertBool "test identifier starting with number" (isLeft $ runParser pIdent "" "1invalid"))
  tIdentDigit       = TestCase (assertEqual "test identifier starting with number" (Right $ TIdent "valid123Identifier") (runParser pIdent "" "valid123Identifier"))

-- ============================================================
-- =                    INDIVIDUAL PARSERS                    = 
-- ============================================================

-- test the lexer function (chaining lexers)
tLexer :: Test
tLexer = TestList [tEmptyInput, tLexerSingle, tLexerAll]
  where
    tEmptyInput = TestCase $ assertEqual "lexing empty string" (Right [TEOF]) (runParser lexer "" "")
    tComments = TestCase $ assertEqual "lexing comment" (Right [TEOF]) (runParser lexer "" "# comment")
    tLexerSingle = TestCase $ assertEqual "lexing integer" (Right [TInt 1, TEOF]) (runParser lexer "" "1")
    -- lex a long string containing all types of tokens
    tLexerAll = TestCase $ assertEqual "lexing many" (Right [TLParen,TRWord "int",TInt 123,TRParen,TBinOp "&",TLBrace,TRWord "str",TString "mystring",TRBrace,TBinOp "|",TRWord "bool",TRWord "true",TBinOp "+",TIdent "myIdentifier",TBinOp "/",TRWord "null",TSemi,TRWord "if",TRWord "else",TBinOp "==",TRWord "while",TBinOp "!=",TRWord "print",TBinOp ">=",TRWord "println",TEOF]) (runParser lexer "" "(int 123) & {str \"mystring\"} | bool true + myIdentifier / null; # keywords if else == while != print >= println ")

tests :: Test
tests = TestList [tPrimitives, tRWord, tBinOp, tSymbol, tIdent]

main :: IO ()
main = runTestTTAndExit tests
