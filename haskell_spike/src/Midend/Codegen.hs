{-# LANGUAGE ImportQualifiedPost #-}

module Midend.Codegen (generateLLVM) where

import Control.Monad.State (MonadState (get, put), MonadTrans (lift), State, evalState, void, when)
import Data.Map qualified as M
import Data.Text (Text)
import LLVM.AST qualified as AST
import LLVM.AST.Global (Global (..))
import LLVM.AST.Type (i32, i8, ptr)
import LLVM.IRBuilder qualified as IR
import Midend.Helpers
import Parser.ParserTypes (BOp (..), Type (..))
import Parser.SemantParserTypes (SAst, SExpr, SExpr' (..), SStatement (..), body)

-- ========================================
-- =                 TYPES                =
-- ========================================

-- map var_name: pointer
type SymbolTable = M.Map Text AST.Operand

-- need state to maintain the symbol table
type Builder = IR.IRBuilderT (State SymbolTable)

-- generate LLVM IR for a semantically-typed expression
-- return an LLVM Operand
codegenSexpr :: SExpr -> Builder AST.Operand
codegenSexpr (TyInt, SInt i) = pure $ IR.int32 i
-- booleans represented as a bit (1 = true, 0 = false)
codegenSexpr (TyBool, SBool b) = pure $ IR.bit (if b then 1 else 0)
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
    Assign -> case l of
      (_, SIdent vname) -> do
        table <- lift get
        -- get the value (pointer) at key=vname, error if key doesn't exist
        let vptr = table M.! vname
        IR.store vptr 0 r'
        pure r'
      _ -> error "assignment LHS must be of type SIdent"
codegenSexpr (_, SIdent vname) = do
  table <- lift get
  -- get the value (pointer) at key=vname, error if key doesn't exist
  let vptr = table M.! vname
  IR.load vptr 0
codegenSexpr (_, SPrint inner) =
  case inner of
    (TyInt, _) -> do
      -- get the "%d\n" global string variable
      let fmtOp = getGlobalAsOp (ptr i8) "intFmt"
      exp' <- codegenSexpr inner
      IR.call printfOp [(fmtOp, []), (exp', [])]
    (TyBool, _) -> do
      -- get the "%d\n" global string variable
      let fmtOp = getGlobalAsOp (ptr i8) "intFmt"
      exp' <- codegenSexpr inner
      IR.call printfOp [(fmtOp, []), (exp', [])]
    (TyNull, _) -> error "cannot print null value"

-- generate LLVM IR for a semantically-typed statement
-- return nothing
codegenStatement :: SStatement -> Builder ()
codegenStatement (SStmtExpr e) = void $ codegenSexpr e
codegenStatement (SStmtBlock stmts) = mapM_ codegenStatement stmts
codegenStatement (SStmtVarDecl ty vName val) = void $ do
  -- get the table and validate variable not-yet declared
  table <- lift get
  when (M.member vName table) $ error "cannot re-declare existing variable."
  -- allocate space and store the value
  varPtr <- IR.alloca (toLLVMType ty) Nothing 0
  initVal <- codegenSexpr val
  IR.store varPtr 0 initVal
  -- put the vname:pointer pair into the table of the inner monad (State SymboLTable)
  lift $ put $ M.insert vName varPtr table

-- generate LLVM IR for the definition of the `main` function (runs when program starts)
mainFunction :: SAst -> AST.Definition
mainFunction program =
  AST.GlobalDefinition $
    AST.functionDefaults
      { name = AST.Name "main",
        parameters = ([], False),
        returnType = i32,
        -- use execIRBuilderT to be in the same monadic context as codegenStatement
        basicBlocks = evalState (IR.execIRBuilderT IR.emptyIRBuilder generateMainFunc) M.empty
      }
  where
    -- builder for [Basic Block], uses statements from the semantic AST
    generateMainFunc :: Builder ()
    generateMainFunc = do
      entry <- IR.freshName "entry"
      IR.emitBlockStart entry
      mapM_ codegenStatement (body program)
      IR.ret $ IR.int32 0

-- generate the whole LLVM module from a semantically-typed AST
generateLLVM :: SAst -> AST.Module
generateLLVM program = IR.buildModule "myModule" $ do
  -- define builtin globals
  IR.emitDefn printfDefn
  IR.emitDefn $ makeStringVar "intFmt" "%d\n"
  -- define the main function
  IR.emitDefn $ mainFunction program
