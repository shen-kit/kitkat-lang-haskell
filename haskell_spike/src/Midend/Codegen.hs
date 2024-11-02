{-# LANGUAGE ImportQualifiedPost #-}

module Midend.Codegen (generateLLVM) where

import Control.Monad.State (MonadState (get, put), MonadTrans (lift), State, evalState, modify, void)
import Data.Map qualified as Map
import Data.Text (Text)
import Foreign (alloca)
import LLVM.AST qualified as AST
import LLVM.AST.AddrSpace (AddrSpace (AddrSpace))
import LLVM.AST.Global (Global (..))
import LLVM.AST.Type (i32, i8, ptr)
import LLVM.IRBuilder (IRBuilderT (IRBuilderT))
import LLVM.IRBuilder qualified as IR
import Midend.Helpers
import Parser.ParserTypes (BOp (..), Type (..))
import Parser.SemantParserTypes (SExpr, SExpr' (..), SProgram, SStatement (..))

-- map variable names to pointers
type SymbolTable = Map.Map Text AST.Operand

-- need state to maintain the symbol table
type Builder = IR.IRBuilderT (State SymbolTable)

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

codegenStatement :: SStatement -> Builder ()
codegenStatement (SStmtExpr e) = void $ codegenSexpr e
codegenStatement (SStmtBlock stmts) = mapM_ codegenStatement stmts
codegenStatement (SStmtVarDecl ty vName val) = void $ do
  ptr <- IR.alloca (toLLVMType ty) Nothing 0
  table <- lift get
  initVal <- codegenSexpr val
  IR.store ptr 0 initVal
  lift $ put $ Map.insert vName ptr table

mainFunction :: SProgram -> AST.Definition
mainFunction stmts =
  AST.GlobalDefinition $
    AST.functionDefaults
      { name = AST.Name "main",
        parameters = ([], False),
        returnType = i32,
        -- use execIrBuilderT to be in the same monadic context as codegenStatement
        basicBlocks = evalState (IR.execIRBuilderT IR.emptyIRBuilder (generateMainFunc stmts)) Map.empty
      }
  where
    generateMainFunc :: SProgram -> Builder ()
    generateMainFunc stmts = do
      entry <- IR.freshName "entry"
      IR.emitBlockStart entry
      mapM_ codegenStatement stmts
      IR.ret $ IR.int32 0

generateLLVM :: SProgram -> AST.Module
generateLLVM prog = IR.buildModule "myModule" $ do
  IR.emitDefn printfDefn
  IR.emitDefn $ makeStringVar "intFmt" "%d\n"
  IR.emitDefn $ mainFunction prog
