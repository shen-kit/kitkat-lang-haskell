{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit
import Text.Megaparsec
import Parser.ParserInner
import Parser.ParserTypes
import Lexer.TokenTypes
import TestUtils

-- ============================================================
-- =                       TYPE PARSERS                       = 
-- ============================================================

tType :: Test
tType = TestList [tIntType, tNullType, tBoolType, tStrType]
  where
    tIntType = testParseEq pType [TRWord "int"] $ TyInt
    tNullType = testParseEq pType [TRWord "null"] $ TyNull
    tBoolType = testParseEq pType [TRWord "bool"] $ TyBool
    tStrType = testParseEq pType [TRWord "str"] $ TyStr

-- ============================================================
-- =                    EXPRESSION PARSERS                    = 
-- ============================================================

tExpr :: Test
tExpr = TestList [tIdent, tPrint, tPrintln, tLiterals, tBrackets, tBinOps1, tBinOpsSequence]
  where
    tIdent = testParseEq pExpr [TIdent "myIdentifier"] $ EIdent "myIdentifier"
    tPrint = testParseEq pExpr [TRWord "print", TLParen, TInt 123, TRParen] $ EPrint False (EInt 123)
    tPrintln = testParseEq pExpr [TRWord "println", TLParen, TInt 123, TRParen] $ EPrint True (EInt 123)
    -- literal parsers
    tLiterals = TestList [
        testParseEq pExpr [TInt 0] $ EInt 0, -- 0
        testParseEq pExpr [TRWord "false"] $ EBool False, -- false
        testParseEq pExpr [TString "mystring"] $ EString "mystring" -- "mystring"
      ]
    tBrackets = TestList [
        testParseEq pExpr [TLParen, TInt 0, TRParen] $ EInt 0, -- (1)
        testParseEq pExpr [TLParen, TLParen, TInt 0, TRParen, TRParen] $ EInt 0 -- ((1))
      ]
    -- individual binary operators
    tBinOps1 = TestList [
        testParseEq pExpr [TInt 1, TBinOp "*", TInt 2] $ EBinOp Multiply (EInt 1) (EInt 2),
        testParseEq pExpr [TInt 1, TBinOp "/", TInt 2] $ EBinOp Divide (EInt 1) (EInt 2),
        testParseEq pExpr [TInt 1, TBinOp "+", TInt 2] $ EBinOp Plus (EInt 1) (EInt 2),
        testParseEq pExpr [TInt 1, TBinOp "-", TInt 2] $ EBinOp Minus (EInt 1) (EInt 2),
        testParseEq pExpr [TIdent "x", TBinOp ">=", TInt 2] $ EBinOp Ge (EIdent "x") (EInt 2),
        testParseEq pExpr [TIdent "x", TBinOp "<=", TInt 2] $ EBinOp Le (EIdent "x") (EInt 2),
        testParseEq pExpr [TIdent "x", TBinOp ">", TInt 2] $ EBinOp Gt (EIdent "x") (EInt 2),
        testParseEq pExpr [TIdent "x", TBinOp "<", TInt 2] $ EBinOp Lt (EIdent "x") (EInt 2),
        testParseEq pExpr [TInt 1, TBinOp "==", TInt 2] $ EBinOp Eq (EInt 1) (EInt 2),
        testParseEq pExpr [TInt 1, TBinOp "!=", TInt 2] $ EBinOp NEq (EInt 1) (EInt 2),
        testParseEq pExpr [TInt 1, TBinOp "&", TInt 2] $ EBinOp LAnd (EInt 1) (EInt 2),
        testParseEq pExpr [TInt 1, TBinOp "|", TInt 2] $ EBinOp LOr (EInt 1) (EInt 2),
        testParseEq pExpr [TIdent "x", TBinOp "=", TInt 5] $ EBinOp Assign (EIdent "x") (EInt 5)
      ]
    -- sequences binary operators
    tBinOpsSequence = TestList [
        -- 1 + 2 * 3 -> 1 + (2 * 3)
        testParseEq pExpr [TInt 1, TBinOp "+", TInt 2, TBinOp "*", TInt 3] $
          EBinOp Plus (EInt 1) (EBinOp Multiply (EInt 2) (EInt 3)),
        -- 4 / 2 - 1 -> (4 / 2) - 1
        testParseEq pExpr [TInt 4, TBinOp "/", TInt 2, TBinOp "-", TInt 1] $
          EBinOp Minus (EBinOp Divide (EInt 4) (EInt 2)) (EInt 1),
        -- (5 + 3) * 2 -> (5 + 3) * 2
        testParseEq pExpr [TLParen, TInt 5, TBinOp "+", TInt 3, TRParen, TBinOp "*", TInt 2] $
          EBinOp Multiply (EBinOp Plus (EInt 5) (EInt 3)) (EInt 2),
        -- 7 > 3 & 2 == 2 -> (7 > 3) & (2 == 2)
        testParseEq pExpr [TInt 7, TBinOp ">", TInt 3, TBinOp "&", TInt 2, TBinOp "==", TInt 2] $
          EBinOp LAnd (EBinOp Gt (EInt 7) (EInt 3)) (EBinOp Eq (EInt 2) (EInt 2)),
        -- 1 + 2 * 3 - 4 -> (1 + (2 * 3)) - 4
        testParseEq pExpr [TInt 1, TBinOp "+", TInt 2, TBinOp "*", TInt 3, TBinOp "-", TInt 4] $
          EBinOp Minus (EBinOp Plus (EInt 1) (EBinOp Multiply (EInt 2) (EInt 3))) (EInt 4),
        -- 8 / (2 + 2) -> 8 / (2 + 2)
        testParseEq pExpr [TInt 8, TBinOp "/", TLParen, TInt 2, TBinOp "+", TInt 2, TRParen] $
          EBinOp Divide (EInt 8) (EBinOp Plus (EInt 2) (EInt 2)),
        -- (1 + 2) * (3 - 4) -> (1 + 2) * (3 - 4)
        testParseEq pExpr [TLParen, TInt 1, TBinOp "+", TInt 2, TRParen, TBinOp "*", TLParen, TInt 3, TBinOp "-", TInt 4, TRParen] $
          EBinOp Multiply (EBinOp Plus (EInt 1) (EInt 2)) (EBinOp Minus (EInt 3) (EInt 4))
      ]

-- ===========================================================
-- =                    STATEMENT PARSERS                    =
-- ===========================================================

tStmt :: Test
tStmt = TestList [tExprStmt, tVarDecl, tBlock, tIf, tWhile]
  where
    -- test individual expression statements
    tExprStmt = TestList [
        testParseEq parseStmt [TInt 1, TBinOp "+", TInt 2, TSemi] $
          StmtExpr (EBinOp Plus (EInt 1) (EInt 2)),
        testParseEq parseStmt [TIdent "x", TBinOp "=", TInt 5, TSemi] $
          StmtExpr (EBinOp Assign (EIdent "x") (EInt 5))
      ]
    -- test blocks of statements in a pair of curly braces
    tBlock = TestList [
        -- { int x = 5; }
        testParseEq parseStmt [TLBrace, TRWord "int", TIdent "x", TBinOp "=", TInt 5, TSemi, TRBrace] $
          StmtBlock [StmtVarDecl TyInt "x" (EInt 5)],
        -- { 1 + 2; }
        testParseEq parseStmt [TLBrace, TInt 1, TBinOp "+", TInt 2, TSemi, TRBrace] $
          StmtBlock [StmtExpr (EBinOp Plus (EInt 1) (EInt 2))],
        -- { int y = 10; { y = 20; } }
        testParseEq parseStmt [TLBrace, TRWord "int", TIdent "y", TBinOp "=", TInt 10, TSemi, TLBrace, TIdent "y", TBinOp "=", TInt 20, TSemi, TRBrace, TRBrace] $
          StmtBlock [StmtVarDecl TyInt "y" (EInt 10), StmtBlock [StmtExpr (EBinOp Assign (EIdent "y") (EInt 20))]]
      ]
    -- test variable declaration statements
    tVarDecl = TestList [
        -- int x = 5;
        testParseEq parseStmt [TRWord "int", TIdent "x", TBinOp "=", TInt 5, TSemi] $
          StmtVarDecl TyInt "x" (EInt 5),
        -- bool flag = true;
        testParseEq parseStmt [TRWord "bool", TIdent "flag", TBinOp "=", TRWord "true", TSemi] $
          StmtVarDecl TyBool "flag" (EBool True)
      ]
    -- test if/else statements
    tIf = TestList [
        -- if (x > 0) { int y = 1; }
        testParseEq parseStmt [TRWord "if", TLParen, TIdent "x", TBinOp ">", TInt 0, TRParen, TLBrace, TRWord "int", TIdent "y", TBinOp "=", TInt 1, TSemi, TRBrace] $
          StmtIf (EBinOp Gt (EIdent "x") (EInt 0)) (StmtBlock [StmtVarDecl TyInt "y" (EInt 1)]) (StmtBlock []),
        -- if (true) { x = 5; } else { x = 0; }
        testParseEq parseStmt [TRWord "if", TLParen, TRWord "true", TRParen, TLBrace, TIdent "x", TBinOp "=", TInt 5, TSemi, TRBrace, TRWord "else", TLBrace, TIdent "x", TBinOp "=", TInt 0, TSemi, TRBrace] $
          StmtIf (EBool True) (StmtBlock [StmtExpr (EBinOp Assign (EIdent "x") (EInt 5))]) (StmtBlock [StmtExpr (EBinOp Assign (EIdent "x") (EInt 0))])
      ]
    -- test while loops
    tWhile = TestList [
        -- while (i < 10) { i = i + 1; }
        testParseEq parseStmt [TRWord "while", TLParen, TIdent "i", TBinOp "<", TInt 10, TRParen, TLBrace, TIdent "i", TBinOp "=", TIdent "i", TBinOp "+", TInt 1, TSemi, TRBrace] $
          StmtWhile (EBinOp Lt (EIdent "i") (EInt 10)) (StmtBlock [StmtExpr (EBinOp Assign (EIdent "i") (EBinOp Plus (EIdent "i") (EInt 1)))])
      ]

-- ==========================================================
-- =                     PROGRAM PARSER                     =
-- ==========================================================

tProgram :: Test
tProgram = TestList [tEmptyProgram, tSingleStmtProgram, tMultipleStmtsProgram, tNestedProgram, tEOFProgram]
  where
    -- empty program
    tEmptyProgram = testParseEq parseProgram [TEOF] $ Ast []

    -- single-statement program
    tSingleStmtProgram = testParseEq parseProgram [TRWord "int", TIdent "x", TBinOp "=", TInt 5, TSemi, TEOF] $
      Ast [StmtVarDecl TyInt "x" (EInt 5)]

    -- multi-statement program
    tMultipleStmtsProgram = testParseEq parseProgram 
      [ TRWord "int", TIdent "x", TBinOp "=", TInt 5, TSemi
      , TIdent "x", TBinOp "=", TIdent "x", TBinOp "+", TInt 1, TSemi
      , TEOF ] $
      Ast [ StmtVarDecl TyInt "x" (EInt 5)
          , StmtExpr (EBinOp Assign (EIdent "x") (EBinOp Plus (EIdent "x") (EInt 1)))
          ]

    -- nested statements
    tNestedProgram = testParseEq parseProgram 
      [ TRWord "int", TIdent "x", TBinOp "=", TInt 0, TSemi
      , TRWord "while", TLParen, TIdent "x", TBinOp "<", TInt 5, TRParen
      , TLBrace
      , TIdent "x", TBinOp "=", TIdent "x", TBinOp "+", TInt 1, TSemi
      , TRBrace
      , TEOF ] $
      Ast [ StmtVarDecl TyInt "x" (EInt 0)
          , StmtWhile (EBinOp Lt (EIdent "x") (EInt 5)) 
              (StmtBlock [StmtExpr (EBinOp Assign (EIdent "x") (EBinOp Plus (EIdent "x") (EInt 1)))])
          ]

    -- fails if no EOF token
    tEOFProgram = testParseFail parseProgram [TRWord "int", TIdent "y", TBinOp "=", TInt 10, TSemi]

-- ============================================================
-- =                      RUN ALL TESTS                       =
-- ============================================================

tests :: Test
tests = TestList [tType, tExpr, tStmt, tProgram]

main :: IO ()
main = runTestTTAndExit tests
