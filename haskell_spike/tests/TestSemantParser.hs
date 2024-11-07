{-# LANGUAGE OverloadedStrings #-}

module Main where

import TestUtils
import Data.Either
import Test.HUnit
import Text.Megaparsec
import Parser.SemantParserInner
import Parser.SemantParserTypes
import Parser.ParserTypes
import Control.Monad.State
import Data.Map as M
import Control.Monad.Except

-- ============================================================
-- =              TEST TYPE-CHECKING EXPRESSIONS              = 
-- ============================================================

tCheckExpr = TestList [tLiteral, tBinaryOps, tBooleanOps, tAssignment, tIdent, tPrint]
  where
    tLiteral = TestList [
        testSemantEq checkExpr (EInt 1) $ (TyInt, SInt 1), -- int
        testSemantEq checkExpr (EBool True) $ (TyBool, SBool True), -- bool
        testSemantEq checkExpr (EString "mystring") $ (TyStr, SStr "mystring") -- string
      ]

    -- numeric binary operations
    tBinaryOps = TestList [
        -- 1 + 2
        testSemantEq checkExpr (EBinOp Plus (EInt 1) (EInt 2)) (TyInt, SBinOp Plus (TyInt, SInt 1) (TyInt, SInt 2)),
        -- 3 * 4
        testSemantEq checkExpr (EBinOp Multiply (EInt 3) (EInt 4)) (TyInt, SBinOp Multiply (TyInt, SInt 3) (TyInt, SInt 4)),
        -- 1 < 2
        testSemantEq checkExpr (EBinOp Lt (EInt 1) (EInt 2)) (TyBool, SBinOp Lt (TyInt, SInt 1) (TyInt, SInt 2)),
        -- 5 > 3
        testSemantEq checkExpr (EBinOp Gt (EInt 5) (EInt 3)) (TyBool, SBinOp Gt (TyInt, SInt 5) (TyInt, SInt 3)),
        -- 10 == 10
        testSemantEq checkExpr (EBinOp Eq (EInt 10) (EInt 10)) (TyBool, SBinOp Eq (TyInt, SInt 10) (TyInt, SInt 10))
      ]

    -- boolean binary operations
    tBooleanOps = TestList [
        -- true & false
        testSemantEq checkExpr (EBinOp LAnd (EBool True) (EBool False)) (TyBool, SBinOp LAnd (TyBool, SBool True) (TyBool, SBool False)),
        -- true | false
        testSemantEq checkExpr (EBinOp LOr (EBool True) (EBool False)) (TyBool, SBinOp LOr (TyBool, SBool True) (TyBool, SBool False))
      ]

    -- test variable assignment
    tAssignment = TestList [
        testSemantEq checkExpr (EBinOp Assign (EIdent "myVar") (EInt 5)) (TyInt, SBinOp Assign (TyInt, SIdent "myVar") (TyInt, SInt 5))
      ]

    -- Identifier test (e.g., using a previously declared variable)
    tIdent = TestList [
        testSemantEq checkExpr (EIdent "myVar") (TyInt, SIdent "myVar")
      ]

    -- Print expressions test (e.g., `print` and `println`)
    tPrint = TestList [
        testSemantEq checkExpr (EPrint (EBool False) (EInt 42)) (TyNull, SPrint (TyBool, SBool False) (TyInt, SInt 42)),
        testSemantEq checkExpr (EPrint (EBool True) (EString "hello")) (TyNull, SPrint (TyBool, SBool True) (TyStr, SStr "hello"))
      ]

-- ============================================================
-- =              TEST TYPE-CHECKING STATEMENTS               = 
-- ============================================================

tCheckStmt :: Test
tCheckStmt = TestList [tExprStmt, tVarDecl, tBlock, tIfStmt, tWhileStmt]
  where
    -- test single-expression statements
    tExprStmt = TestList
      [ testSemantEq checkStatement (StmtExpr (EInt 1)) (SStmtExpr (TyInt, SInt 1)),
        testSemantEq checkStatement (StmtExpr (EBool True)) (SStmtExpr (TyBool, SBool True))
      ]

    -- test variable declaration statements
    tVarDecl = TestList
      [ testSemantEq checkStatement (StmtVarDecl TyInt "x" (EInt 5)) (SStmtVarDecl TyInt "x" (TyInt, SInt 5)),
        testSemantEq checkStatement (StmtVarDecl TyBool "flag" (EBool False)) (SStmtVarDecl TyBool "flag" (TyBool, SBool False))
      ]

    -- test block statements (+nesting)
    tBlock = TestList [
        -- { 1; 2; }
        testSemantEq checkStatement (StmtBlock [StmtExpr (EInt 1), StmtExpr (EInt 2)]) 
                      (SStmtBlock [SStmtExpr (TyInt, SInt 1), SStmtExpr (TyInt, SInt 2)]),
        -- { 3; { 4; 5; } }
        testSemantEq checkStatement (StmtBlock [StmtExpr (EInt 3), StmtBlock [StmtExpr (EInt 4), StmtExpr (EInt 5)]])
                      (SStmtBlock [SStmtExpr (TyInt, SInt 3), SStmtExpr (TyInt, SInt 4), SStmtExpr (TyInt, SInt 5)])
      ]

    -- test if statements
    tIfStmt = TestList [
        -- if (true) 1; else 2;
        testSemantEq checkStatement (StmtIf (EBool True) (StmtExpr (EInt 1)) (StmtExpr (EInt 2))) 
                      (SStmtIf (TyBool, SBool True) (SStmtExpr (TyInt, SInt 1)) (SStmtExpr (TyInt, SInt 2))),
        -- if false 1 else {2; 3;}
        testSemantEq checkStatement (StmtIf (EBool False) (StmtExpr (EInt 1)) (StmtBlock [StmtExpr (EInt 2), StmtExpr (EInt 3)])) 
                      (SStmtIf (TyBool, SBool False) (SStmtExpr (TyInt, SInt 1)) (SStmtBlock [SStmtExpr (TyInt, SInt 2), SStmtExpr (TyInt, SInt 3)]))
      ]

    -- test while loops
    tWhileStmt = TestList [
        -- while (true) 1;
        testSemantEq checkStatement (StmtWhile (EBool True) (StmtExpr (EInt 1))) 
                      (SStmtWhile (TyBool, SBool True) (SStmtExpr (TyInt, SInt 1))),
        -- while (false) { 2; 3; }
        testSemantEq checkStatement (StmtWhile (EBool False) (StmtBlock [StmtExpr (EInt 2), StmtExpr (EInt 3)])) 
                      (SStmtWhile (TyBool, SBool False) (SStmtBlock [SStmtExpr (TyInt, SInt 2), SStmtExpr (TyInt, SInt 3)]))
      ]

-- test retrieving variable type from the table (requires variable "myVar" to exist in state table)
tCheckVarType = testSemantEq checkVarType "myVar" TyInt

-- ===========================================================
-- =            TEST TYPE-CHECKING WHOLE PROGRAM             = 
-- ===========================================================

tCheckProgram :: Test
tCheckProgram = TestList [tValidProgram]
  where
    tValidProgram = let
          ast = Ast 
            [ StmtVarDecl TyInt "x" (EInt 1),                               -- int x = 1;
              StmtVarDecl TyBool "flag" (EBool True),                       -- bool flag = true;
              StmtExpr (EBinOp Plus (EIdent "x") (EInt 2)),                 -- x + 2;
              StmtIf (EBool True) (StmtExpr (EInt 3)) (StmtExpr (EInt 4)),  -- if (true) then 3 else 4;
              StmtWhile (EBool False) (StmtExpr (EInt 5))                   -- while (false) 5;
            ]
          expectedSAst = SAst 
            { body = [ SStmtVarDecl TyInt "x" (TyInt, SInt 1),
                       SStmtVarDecl TyBool "flag" (TyBool, SBool True),
                       SStmtExpr (TyInt, SBinOp Plus (TyInt, SIdent "x") (TyInt, SInt 2)),
                       SStmtIf (TyBool, SBool True) (SStmtExpr (TyInt, SInt 3)) (SStmtExpr (TyInt, SInt 4)),
                       SStmtWhile (TyBool, SBool False) (SStmtExpr (TyInt, SInt 5))
                     ],
              vars = M.fromList [("x", TyInt), ("flag", TyBool)],
              funcs = []
            }
      in TestCase $ assertEqual "" (Right expectedSAst) (checkProgram ast)

-- =======================================================
-- =                    RUN ALL TESTS                    =
-- =======================================================

tests :: Test
tests = TestList [tCheckExpr, tCheckStmt, tCheckVarType, tCheckProgram]

main :: IO ()
main = runTestTTAndExit tests
