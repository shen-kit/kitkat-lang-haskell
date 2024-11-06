{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Midend.Codegen (generateLLVM) where

import Control.Monad.State (MonadState, State, evalState, gets, modify, void, when)
import Data.Map qualified as M
import Data.Text (Text)
import LLVM.AST (Instruction (function))
import LLVM.AST qualified as AST
import LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Type (i32, i8, ptr)
import LLVM.IRBuilder qualified as IR
import Midend.Helpers
import Parser.ParserTypes (BOp (..), Type (..))
import Parser.SemantParserTypes (SAst (funcs), SExpr, SExpr' (..), SStatement (..), body)

-- ========================================
-- =                 TYPES                =
-- ========================================

-- strings => str_literal : pointer
-- vars    => var_name    : pointer
data Env = Env
  { strings :: M.Map Text AST.Operand,
    operands :: M.Map Text AST.Operand
  }
  deriving (Eq, Show)

-- need state to maintain the already-declared variables and strings
type LLVM = IR.ModuleBuilderT (State Env)

-- IRBuilderT needs to include the ModuleBuilderT functionality for globalStringPtr to work
type Builder = IR.IRBuilderT LLVM

registerOperand :: (MonadState Env m) => Text -> AST.Operand -> m ()
registerOperand name op = modify $ \env -> env {operands = M.insert name op (operands env)}

-- ============================================================
-- =                EXPRESSION CODE-GENERATION                =
-- ============================================================

-- generate LLVM IR for a semantically-typed expression
-- return an LLVM Operand
codegenSexpr :: SExpr -> Builder AST.Operand
codegenSexpr (TyInt, SInt i) = pure $ IR.int32 i
-- booleans represented as a bit (1 = true, 0 = false)
codegenSexpr (TyBool, SBool b) = pure $ IR.bit (if b then 1 else 0)
codegenSexpr (TyStr, SStr s) = do
  strs <- gets strings
  case M.lookup s strs of
    -- create new global string variable
    Just op -> pure op
    Nothing -> do
      let name' = AST.mkName $ "_str" <> show (M.size strs)
      const' <- IR.globalStringPtr (tts s) name'
      let op = AST.ConstantOperand const'
      modify $ \env -> env {strings = M.insert s op strs}
      pure op
codegenSexpr (t, SBinOp op l r) = do
  l' <- codegenSexpr l
  r' <- codegenSexpr r
  case op of
    Plus -> case t of
      TyInt -> IR.add l' r'
      _ -> error $ "cannot add type: " ++ show t
    Minus -> case t of
      TyInt -> IR.sub l' r'
      _ -> error $ "cannot add type: " ++ show t
    Multiply -> case t of
      TyInt -> IR.mul l' r'
      _ -> error $ "cannot add type: " ++ show t
    Divide -> case t of
      TyInt -> IR.sdiv l' r'
      _ -> error $ "cannot add type: " ++ show t
    Assign -> case l of
      (_, SIdent vname) -> do
        table <- gets operands
        -- get the value (pointer) at key=vname, error if key doesn't exist
        let vptr = table M.! vname
        IR.store vptr 0 r'
        pure r'
      _ -> error "assignment LHS must be of type SIdent"
    Lt -> case fst l of
      TyInt -> IR.icmp IP.SLT l' r'
      _ -> error "can only compare integers"
    Gt -> case fst l of
      TyInt -> IR.icmp IP.SGT l' r'
      _ -> error "can only compare integers"
    Le -> case fst l of
      TyInt -> IR.icmp IP.SLE l' r'
      _ -> error "can only compare integers"
    Ge -> case fst l of
      TyInt -> IR.icmp IP.SGE l' r'
      _ -> error "can only compare integers"
    Eq -> case fst l of
      TyInt -> IR.icmp IP.EQ l' r'
      _ -> error "can only compare integers"
    NEq -> case fst l of
      TyInt -> IR.icmp IP.NE l' r'
      _ -> error "can only compare integers"
    LAnd -> IR.and l' r'
    LOr -> IR.or l' r'
codegenSexpr (_, SIdent vname) = do
  table <- gets operands
  -- get the value (pointer) at key=vname, error if key doesn't exist
  let vptr = table M.! vname
  IR.load vptr 0
codegenSexpr (_, SPrint (TyBool, SBool ln) inner) = do
  case inner of
    (TyInt, _) -> do
      -- get the "%d\n" global string variable
      let fmtName = if ln then "intFmtLn" else "intFmt"
      let fmtOp = getGlobalAsOp (ptr i8) fmtName
      exp' <- codegenSexpr inner
      IR.call printfOp [(fmtOp, []), (exp', [])]
    (TyBool, _) -> do
      -- get the "%d\n" global string variable
      let fmtName = if ln then "intFmtLn" else "intFmt"
      let fmtOp = getGlobalAsOp (ptr i8) fmtName
      exp' <- codegenSexpr inner
      IR.call printfOp [(fmtOp, []), (exp', [])]
    (TyStr, _) -> do
      let fmtName = if ln then "strFmtLn" else "strFmt"
      let fmtOp = getGlobalAsOp (ptr i8) fmtName
      exp' <- codegenSexpr inner
      IR.call printfOp [(fmtOp, []), (exp', [])]
    (TyNull, _) -> error "cannot print null value"
codegenSexpr (_, SCall fn args) = do
  args' <- mapM (fmap (,[]) . codegenSexpr) args
  f <- gets ((M.! fn) . operands)
  IR.call f args'
codegenSexpr s = error $ "cannot generate LLVM IR code for the semantic expression: " ++ show s

-- ============================================================
-- =                 STATEMENT CODE-GENERATION                =
-- ============================================================

-- generate LLVM IR for a semantically-typed statement
-- return nothing
codegenStatement :: SStatement -> Builder ()
codegenStatement (SStmtExpr e) = void $ codegenSexpr e
codegenStatement (SStmtBlock stmts) = mapM_ codegenStatement stmts
codegenStatement (SStmtVarDecl ty vName val) = void $ do
  -- get the table and validate variable not-yet declared
  table <- gets operands
  when (M.member vName table) $ error "cannot re-declare existing variable."
  -- allocate space and store the value
  varPtr <- IR.alloca (toLLVMType ty) Nothing 0
  initVal <- codegenSexpr val
  IR.store varPtr 0 initVal
  -- put the vname:pointer pair into the vars table of the inner monad (State Env)
  registerOperand vName varPtr

-- generate code for if/else if/else blocks, using conditional branch
codegenStatement (SStmtIf cond ifBody elseBody) = mdo
  cond' <- codegenSexpr cond
  IR.condBr cond' ifBlock elseBlock

  -- generate BasicBlock for success condition
  ifBlock <- IR.block `IR.named` "then"
  codegenStatement ifBody
  IR.br mergeBlock
  -- generate BasicBlock for fail condition
  elseBlock <- IR.block `IR.named` "else"
  codegenStatement elseBody
  IR.br mergeBlock

  -- both if/else return to the 'merge block' as each function can only have one return
  mergeBlock <- IR.block `IR.named` "merge"
  pure ()

-- generate code for while loop using conditional branch
codegenStatement (SStmtWhile cond body') = mdo
  -- check if condition is true the first time running
  cond' <- codegenSexpr cond
  IR.condBr cond' bodyBlock mergeBlock

  -- basic block for loop body
  bodyBlock <- IR.block `IR.named` "body"
  codegenStatement body'
  continue <- codegenSexpr cond -- re-evaluate condition
  IR.condBr continue bodyBlock mergeBlock

  -- termination block
  mergeBlock <- IR.block `IR.named` "body"
  pure ()

-- ============================================================
-- =                 MAIN FUNC CODE-GENERATION                =
-- ============================================================

codegenMainFunc :: SAst -> LLVM ()
codegenMainFunc sast = mdo
  strs <- do
    _ <- IR.function "main" [] i32 genBody
    -- includes string literals defined in the function body just generated
    gets strings
  modify $ \env -> env {strings = strs}
  where
    genBody :: [AST.Operand] -> Builder ()
    genBody _ = do
      entry <- IR.freshName "entry"
      IR.emitBlockStart entry
      mapM_ codegenStatement (body sast)
      IR.ret $ IR.int32 0

-- ============================================================
-- =                  OVERALL CODE-GENERATION                 =
-- ============================================================

generateLLVM :: SAst -> AST.Module
generateLLVM program =
  flip evalState initState $
    IR.buildModuleT "myModule" $ do
      -- external variadic argument function definition
      printf <- IR.externVarArgs (AST.mkName "printf") [ptr i8] i32
      registerOperand "printf" printf
      -- helper constants
      declareGlobalStr "%s" "strFmt"
      declareGlobalStr "%s\n" "strFmtLn"
      declareGlobalStr "%d" "intFmt"
      declareGlobalStr "%d\n" "intFmtLn"
      codegenMainFunc program
  where
    initState = Env {operands = M.empty, strings = M.empty}

    -- helper: declares global string variable and adds to the environment
    declareGlobalStr str name' = do
      vptr <- IR.globalStringPtr str $ AST.mkName (tts name')
      registerOperand name' $ AST.ConstantOperand vptr
