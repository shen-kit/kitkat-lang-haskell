{-# LANGUAGE ImportQualifiedPost #-}

module Midend.Codegen (generateLLVM) where

import Control.Monad.State (void)
import LLVM.AST qualified as AST
import LLVM.AST.AddrSpace (AddrSpace (AddrSpace))
import LLVM.AST.Global (Global (..))
import LLVM.AST.Type (i32, i8, ptr)
import LLVM.IRBuilder qualified as IR
import Midend.Helpers
import Parser.ParserTypes (BOp (..), Type (..))
import Parser.SemantParserTypes (SExpr, SExpr' (..), SProgram, SStatement (..))

-- non-transformer version of `IR.IRBuilderT`
type Builder = IR.IRBuilder

codegenSexpr :: SExpr -> Builder AST.Operand
codegenSexpr (TyInt, SInt i) = pure $ IR.int32 i
codegenSexpr (t, SBinOp op l r) = do
  l' <- codegenSexpr l
  r' <- codegenSexpr r
  case op of
    Plus -> case t of
      TyInt -> IR.add l' r'
    Minus -> case t of
      TyInt -> IR.sub l' r'
    Multiply -> case t of
      TyInt -> IR.mul l' r'
    Divide -> case t of
      TyInt -> IR.sdiv l' r'
codegenSexpr (_, SPrint inner) =
  case inner of
    (TyInt, _) -> do
      let fmtOp = getGlobalAsOp (ptr i8) "intFmt"
      exp' <- codegenSexpr inner
      IR.call printfOp [(fmtOp, []), (exp', [])]
    (TyNull, _) -> error "cannot print null value"
codegenSexpr a = error $ "unrecognised expression" ++ show a

codegenStatement :: SStatement -> Builder ()
codegenStatement (SStmtExpr e) = void $ codegenSexpr e
codegenStatement (SStmtBlock stmts) = mapM_ codegenStatement stmts

mainFunction :: SProgram -> AST.Definition
mainFunction stmts =
  AST.GlobalDefinition $
    AST.functionDefaults
      { name = AST.Name "main",
        parameters = ([], False),
        returnType = i32,
        basicBlocks = IR.execIRBuilder IR.emptyIRBuilder $ do
          -- get unique name from suggestion
          entry <- IR.freshName "entry"
          -- defines the start of a new block with the given name ("entry")
          IR.emitBlockStart entry
          -- executes each `IRBuilder ()` sequentially, adds instructions to the basic block
          mapM_ codegenStatement stmts
          -- return 0 (function exit success)
          IR.ret $ IR.int32 0
      }

generateLLVM :: SProgram -> AST.Module
generateLLVM prog = IR.buildModule "myModule" $ do
  IR.emitDefn printfDefn
  IR.emitDefn $ makeStringVar "intFmt" "%d\n"
  IR.emitDefn $ makeStringVar "floatFmt" "%f\n"
  IR.emitDefn $ makeStringVar "strFmt" "%s\n"
  IR.emitDefn $ mainFunction prog
