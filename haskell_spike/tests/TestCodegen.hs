{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser.ParserTypes (Type (..))
import Parser.SemantParserTypes
import Midend.CodegenInner

import Control.Monad.State (evalState)
import LLVM.IRBuilder qualified as IR
import LLVM.AST qualified as AST
import Test.HUnit
import Data.Map qualified as M

-- HELPERS

initEnv = Env { strings = M.empty, operands = M.empty }

evalCodegenSexpr :: SExpr -> AST.Operand
evalCodegenSexpr sexpr =
  fst $ fst $ evalState (IR.runModuleBuilderT IR.emptyModuleBuilder (IR.runIRBuilderT IR.emptyIRBuilder (codegenSexpr sexpr))) initEnv

-- TESTS

tests :: Test
tests = TestList [
    "int literal 100" ~: do
      let sexpr = (TyInt, SInt 100)
      let expected = IR.int32 100
      evalCodegenSexpr sexpr @?= expected
  ]

main :: IO ()
main = runTestTTAndExit tests
