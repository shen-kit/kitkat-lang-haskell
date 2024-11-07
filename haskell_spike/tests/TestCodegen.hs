{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser.ParserTypes (Type (..), BOp (..))
import Parser.SemantParserTypes
import Midend.CodegenInner

import Control.Monad.State (evalState)
import LLVM.AST.IntegerPredicate as IP
import LLVM.IRBuilder qualified as IR
import LLVM.AST qualified as AST
import Test.HUnit
import Data.Map qualified as M

-- HELPERS

initialEnv = Env { strings = M.empty, operands = M.empty }

evalCodegenSexpr :: SExpr -> AST.Operand
evalCodegenSexpr sexpr =
  fst $ fst $ evalState (IR.runModuleBuilderT IR.emptyModuleBuilder (IR.runIRBuilderT IR.emptyIRBuilder (codegenSexpr sexpr))) initialEnv

-- TESTS

-- test Operand result of code-generation from semantic expression
testCodegenSexpr :: Test
testCodegenSexpr = TestList [
    -- integers
    "int literal min" ~: evalCodegenSexpr (TyInt, SInt $ -2147483648) @?= IR.int32 (-2147483648),
    "int literal 0" ~: evalCodegenSexpr (TyInt, SInt 0) @?= IR.int32 0,
    "int literal 100" ~: evalCodegenSexpr (TyInt, SInt 100) @?= IR.int32 100,
    "int literal max" ~: evalCodegenSexpr (TyInt, SInt 2147483647) @?= IR.int32 2147483647,

    -- boolean
    "bool literal false" ~: evalCodegenSexpr (TyBool, SBool False) @?= IR.bit 0,
    "bool literal true" ~: evalCodegenSexpr (TyBool, SBool True) @?= IR.bit 1
  ]

tests :: Test
tests = TestList [testCodegenSexpr]

main :: IO ()
main = runTestTTAndExit tests
