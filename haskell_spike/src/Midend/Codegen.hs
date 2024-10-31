{-# LANGUAGE ImportQualifiedPost #-}

module Midend.Codegen (generateLLVM) where

import Control.Monad.State (State, evalState, void)
import Data.String (IsString (fromString))
import LLVM.AST qualified as AST
import LLVM.IRBuilder qualified as L
import Parser.ParserTypes (BOp (..), Type (..))
import Parser.SemantParserTypes (SExpr, SExpr' (..), SProgram, SStatement (..))

-- defines the structure of a module (handles functions and global variables)
-- responsible for overall module state
type LLVM = L.ModuleBuilderT (State SProgram)

-- creates individual instructions within the scope of a basic block
-- manages SSA form
type Codegen = L.IRBuilderT LLVM

codegenSexpr :: SExpr -> Codegen AST.Operand
codegenSexpr (TyInt, SInt i) = pure $ L.int32 (fromIntegral i)
codegenSexpr (t, SBinOp op l r) = do
  l' <- codegenSexpr l
  r' <- codegenSexpr r
  case op of
    Plus -> case (fst l, fst r) of
      (TyInt, TyInt) -> L.add l' r'
    Minus -> case (fst l, fst r) of
      (TyInt, TyInt) -> L.sub l' r'
    Multiply -> case t of
      TyInt -> L.mul l' r'
    Divide -> case t of
      TyInt -> L.mul l' r'

codegenStatement :: SStatement -> Codegen ()
codegenStatement (SStmtExpr e) = void $ codegenSexpr e
codegenStatement (SStmtBlock stmts) = mapM_ codegenStatement stmts

generateLLVM :: SProgram -> AST.Module
generateLLVM stmts = evalState (L.buildModuleT (fromString "moduleName") llvmCodegen) initialState
  where
    initialState = []

    llvmCodegen :: LLVM ()
    llvmCodegen = do
      _ <- L.function (fromString "main") [] (AST.IntegerType 32) $ \[] -> do
        mapM_ codegenStatement stmts
        L.ret (L.int32 0)
      pure ()
