{-# LANGUAGE FlexibleContexts #-}

module Parser.SemantParser where

import Control.Monad.Except
import Control.Monad.State (evalState)
import Parser.ParserTypes (Ast (Ast), BOp (..), Expr (..), Statement (..), Type (..))
import Parser.SemantParserTypes (SAst, SExpr, SExpr' (..), SStatement (..), Semant)

-- converts expressions into semantic expressions using type inferencing
-- also performs type-checking on operations
checkExpr :: Expr -> Semant SExpr
checkExpr (EInt i) = pure (TyInt, SInt i)
checkExpr (EBool b) = pure (TyBool, SBool b)
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
        case op of
          -- use this syntax when more types:
          -- case (t1, t2) of
          --   (TyInt, TyInt) -> pure (TyInt, sexpr)
          --   (Ty1, Ty2) -> pure (..., sexpr)
          Plus -> assertTypeEq >> checkNumeric
          Minus -> assertTypeEq >> checkNumeric
          Multiply -> assertTypeEq >> checkNumeric
          Divide -> assertTypeEq >> checkNumeric
          -- only allow assignment if var & expr have the same type
          Assign -> assertTypeEq >> pure (t1, sexpr)
-- TODO: store variable types and load from table
checkExpr (EIdent vname) = pure (TyInt, SIdent vname)
checkExpr (EPrint inner) = do
  inner'@(t, _) <- checkExpr inner
  case t of
    TyInt -> pure (TyNull, SPrint inner')
    _ -> error $ "cannot print value: " ++ show inner

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
  guard $ actualType == declType
  pure $ SStmtVarDecl declType vName expr'

-- type-check the entire AST by type-checking each statement in it
-- returns Either <error msg> <semantically-typed AST>
checkProgram :: Ast -> Either String SAst
checkProgram ast = evalState (runExceptT (checkProgram' ast)) initAst
  where
    initAst = []
    -- initAst = SAst {stmts = [], vars = M.empty}
    checkProgram' :: Ast -> Semant SAst
    checkProgram' (Ast stmts) = mapM checkStatement stmts
