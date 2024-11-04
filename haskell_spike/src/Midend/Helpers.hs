{-# LANGUAGE ImportQualifiedPost #-}

module Midend.Helpers (module Midend.Helpers) where

import Data.ByteString.Short (ShortByteString)
import LLVM.AST
import LLVM.AST.Constant (Constant (GlobalReference))
import LLVM.AST.Type (i1, i32, i8, ptr, void)
import Parser.ParserTypes qualified as PTypes

-- =====================================================
-- =                    GET GLOBALS                    =
-- =====================================================

-- get a globally defined operand, by specifying its type and name
getGlobalAsOp :: Type -> ShortByteString -> Operand
getGlobalAsOp ty name' = ConstantOperand $ GlobalReference ty (Name name')

-- get the printf function as an operand
printfOp :: Operand
printfOp = getGlobalAsOp (ptr (FunctionType i32 [ptr i8] True)) "printf"

-- =====================================================
-- =                     UTILITIES                     =
-- =====================================================

toLLVMType :: PTypes.Type -> Type
toLLVMType PTypes.TyInt = i32
toLLVMType PTypes.TyBool = i1
toLLVMType PTypes.TyStr = ptr i8
toLLVMType PTypes.TyNull = void
