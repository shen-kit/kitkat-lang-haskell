{-# LANGUAGE FlexibleContexts #-}

module Parser.SemantParserInner where

import Control.Monad.Except
import Control.Monad.State (evalState, gets, modify)
import Data.Map as M
import Data.Text (Text)
import Parser.ParserTypes (Ast (Ast), BOp (..), Expr (..), Statement (..), Type (..))
import Parser.SemantParserTypes (SAst (..), SExpr, SExpr' (..), SStatement (..), Semant)

-- converts expressions into semantic expressions using type inferencing
-- also performs type-checking on operations
checkExpr :: Expr -> Semant SExpr
checkExpr (EInt i) = pure (TyInt, SInt i)
checkExpr (EBool b) = pure (TyBool, SBool b)
checkExpr (EString s) = pure (TyStr, SStr s)
checkExpr (EBinOp op l r) =
  let isNum t = case t of
        TyInt -> True
        _ -> False
   in do
        l'@(t1, _) <- checkExpr l
        r'@(t2, _) <- checkExpr r
        let sexpr = SBinOp op l' r'
            assertTypeEq = unless (t1 == t2) $ throwError "types not equal"
            checkNumeric = do
              unless (isNum t1) $ throwError "expected numeric type"
              pure (t1, sexpr)
            checkBool = do
              when (t1 /= TyBool) $ throwError "expected boolean type"
              pure (t1, sexpr)
            makeComparator = do
              assertTypeEq
              unless (isNum t1) $ throwError "expected numeric type"
              pure (TyBool, sexpr)
        case op of
          -- numeric operators
          Plus -> assertTypeEq >> checkNumeric
          Minus -> assertTypeEq >> checkNumeric
          Multiply -> assertTypeEq >> checkNumeric
          Divide -> assertTypeEq >> checkNumeric
          -- numeric comparators
          Lt -> makeComparator
          Gt -> makeComparator
          Le -> makeComparator
          Ge -> makeComparator
          Eq -> makeComparator
          NEq -> makeComparator
          -- boolean operators
          LAnd -> assertTypeEq >> checkBool
          LOr -> assertTypeEq >> checkBool
          -- only allow assignment if var & expr have the same type
          Assign -> assertTypeEq >> pure (t1, sexpr)
checkExpr (EIdent vname) = do
  vtype <- checkVarType vname
  pure (vtype, SIdent vname)
checkExpr (EPrint ln inner) = do
  inner'@(t, _) <- checkExpr inner
  ln'@(tLn, _) <- checkExpr ln
  when (tLn /= TyBool) $ throwError "second argument to print must be of type boolean"
  case t of
    TyNull -> error $ "cannot print value: " ++ show inner
    _ -> pure (TyNull, SPrint ln' inner')
checkExpr (ECall fn args) = do
  args' <- mapM checkExpr args
  pure (TyNull, SCall fn args')

-- type-check a statement node of the AST, return a typed statement node
checkStatement :: Statement -> Semant SStatement
checkStatement (StmtExpr e) = SStmtExpr <$> checkExpr e
checkStatement (StmtBlock exprs) = do
  let flattened = flatten exprs
  SStmtBlock <$> mapM checkStatement flattened
  where
    flatten [] = []
    flatten (StmtBlock s : xs) = flatten (s ++ xs)
    flatten (s : xs) = s : flatten xs
checkStatement (StmtVarDecl declType vName expr) = do
  expr'@(actualType, _) <- checkExpr expr
  when (actualType /= declType) $ throwError "assignment and expression types differ"
  modify $ \s -> s {vars = M.insert vName actualType (vars s)}
  pure $ SStmtVarDecl declType vName expr'
checkStatement (StmtIf cond ifBody elseBody) = do
  cond'@(condTy, _) <- checkExpr cond
  when (condTy /= TyBool) $ throwError "condition following 'if' must have type boolean"
  ifBody' <- checkStatement ifBody
  elseBody' <- checkStatement elseBody
  pure $ SStmtIf cond' ifBody' elseBody'
checkStatement (StmtWhile cond body') = do
  cond'@(condTy, _) <- checkExpr cond
  when (condTy /= TyBool) $ throwError "condition following 'if' must have type boolean"
  sBody <- checkStatement body'
  pure $ SStmtWhile cond' sBody

checkVarType :: Text -> Semant Type
checkVarType varname = do
  varTable <- gets vars
  pure $ varTable M.! varname

-- type-check the entire AST by type-checking each statement in it
-- returns Either <error msg> <semantically-typed AST>
checkProgram :: Ast -> Either String SAst
checkProgram ast = evalState (runExceptT (checkProgram' ast)) initAst
  where
    initAst = SAst {body = [], vars = M.empty, funcs = []}
    checkProgram' :: Ast -> Semant SAst
    checkProgram' (Ast stmts) = do
      stmts' <- mapM checkStatement stmts
      vars' <- gets vars
      pure $ SAst {body = stmts', vars = vars', funcs = []}
