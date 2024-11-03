{-# LANGUAGE ImportQualifiedPost #-}

module Midend.Helpers (module Midend.Helpers) where

import Data.ByteString.Short (ShortByteString)
import LLVM.AST
import LLVM.AST.Constant (Constant (Array, GlobalReference, Int))
import LLVM.AST.Global as G (Global (..))
import LLVM.AST.Linkage (Linkage (External))
import LLVM.AST.Type (i1, i32, i8, ptr, void)
import Parser.ParserTypes qualified as PTypes

-- =====================================================
-- =                   MAKE GLOBALS                    =
-- =====================================================

-- make a global variable
makeGlobalVar :: ShortByteString -> Bool -> Type -> Maybe Constant -> Definition
makeGlobalVar name' isConst ty initialiser =
  GlobalDefinition $
    globalVariableDefaults
      { name = Name name',
        isConstant = isConst,
        G.type' = ty,
        initializer = initialiser
      }

-- strings are constant, type is i8 array taking '\0' into account
makeStringVar :: ShortByteString -> String -> Definition
makeStringVar name' str =
  makeGlobalVar
    name'
    True
    (ArrayType (fromIntegral $ length str + 1) i8)
    -- fromEnum converts Char -> unicode representation
    -- fromIntegral converts Int -> Integer
    -- Int 8 -> wraps the produced integer a LLVM Int constant of 8 bits
    (Just $ Array i8 (map (Int 8 . fromIntegral . fromEnum) (str ++ "\0")))

-- defines the printf function to link with printf from the standard C library
printfDefn :: Definition
printfDefn =
  GlobalDefinition $
    functionDefaults
      { name = Name "printf",
        linkage = External,
        parameters = ([Parameter (ptr i8) (Name "format") []], False),
        returnType = i32,
        basicBlocks = []
      }

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
toLLVMType PTypes.TyNull = void
