{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Midend.Codegen (generateLLVM) where

import Control.Monad.State (State, evalState, void)
import Data.Functor.Identity (Identity)
import Data.String (IsString (fromString))
import GHC.Float (properFractionFloat)
import LLVM.AST (Definition)
import LLVM.AST qualified as AST
import LLVM.AST.CallingConvention (CallingConvention (C))
import LLVM.AST.Constant (Constant (Array, GlobalReference, Int))
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Type as AST hiding (void)
import LLVM.AST.Visibility
import LLVM.IRBuilder (call)
import LLVM.IRBuilder qualified as AST
import LLVM.IRBuilder qualified as L
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction (globalStringPtr)
import Parser.ParserTypes (BOp (..), Type (..))
import Parser.SemantParserTypes (SExpr, SExpr' (..), SProgram, SStatement (..))
import System.Process (proc)

type Codegen = L.IRBuilderT Identity

codegenSexpr :: AST.Operand -> SExpr -> Codegen AST.Operand
codegenSexpr fmtOp (TyInt, SInt i) = pure $ L.int32 (fromIntegral i)
codegenSexpr fmtOp (t, SBinOp op l r) = do
  l' <- codegenSexpr fmtOp l
  r' <- codegenSexpr fmtOp r
  case op of
    Plus -> case (fst l, fst r) of
      (TyInt, TyInt) -> L.add l' r'
    Minus -> case (fst l, fst r) of
      (TyInt, TyInt) -> L.sub l' r'
    Multiply -> case t of
      TyInt -> L.mul l' r'
    Divide -> case t of
      TyInt -> L.sdiv l' r'
codegenSexpr fmtOp (_, SPrint inner) =
  case inner of
    (TyInt, exp) -> do
      exp' <- codegenSexpr fmtOp inner
      call printfDecl [(fmtOp, []), (exp', [])]

codegenStatement :: AST.Operand -> SStatement -> Codegen ()
codegenStatement fmtOp (SStmtExpr e) = void $ codegenSexpr fmtOp e
codegenStatement fmtOp (SStmtBlock stmts) = mapM_ (codegenStatement fmtOp) stmts

printfDecl :: AST.Operand
printfDecl = AST.ConstantOperand $ GlobalReference (ptr (FunctionType i32 [ptr i8] True)) (AST.Name "printf")

printfFunction :: Definition
printfFunction =
  AST.GlobalDefinition $
    Function
      { name = AST.Name "printf",
        linkage = External,
        parameters = ([Parameter (ptr i8) (AST.Name "format") []], False),
        returnType = i32,
        basicBlocks = [],
        visibility = Default,
        callingConvention = C,
        returnAttributes = [],
        functionAttributes = [],
        alignment = 0,
        garbageCollectorName = Nothing,
        personalityFunction = Nothing,
        prefix = Nothing,
        metadata = []
      }

mainFunction :: AST.Operand -> SProgram -> Definition
mainFunction fmtOp stmts =
  AST.GlobalDefinition $
    Function
      { name = AST.Name "main",
        parameters = ([], False),
        returnType = i32,
        basicBlocks = L.execIRBuilder L.emptyIRBuilder $ do
          entry <- L.freshName "entry"
          L.emitBlockStart entry
          mapM_ (codegenStatement fmtOp) stmts
          L.ret (int32 0),
        linkage = External,
        visibility = Default,
        callingConvention = C,
        returnAttributes = [],
        functionAttributes = [],
        alignment = 0,
        garbageCollectorName = Nothing,
        personalityFunction = Nothing,
        prefix = Nothing,
        metadata = []
      }

generateLLVM :: SProgram -> AST.Module
generateLLVM prog = L.buildModule "myModule" $ do
  L.emitDefn printfFunction
  fmt <- globalStringPtr "%d\n" "fmt"
  let fmtOperand = AST.ConstantOperand fmt
  L.emitDefn (mainFunction fmtOperand prog)
