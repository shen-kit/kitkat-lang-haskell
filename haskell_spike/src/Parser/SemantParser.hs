{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Parser.SemantParser where

import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.State (evalState)
import Parser.Parser
import Parser.ParserTypes (BOp (..), Expr (..), Program (Program), Statement (StmtBlock, StmtExpr), Type (..))
import Parser.SemantParserTypes (SExpr, SExpr' (..), SProgram, SStatement (SStmtBlock, SStmtExpr), Semant, SemantError (TypeError))

-- converts expressions into semantic expressions using type inferencing
-- also performs type-checking on operations
checkExpr :: Expr -> Semant SExpr
checkExpr expr = case expr of
  EInt i -> pure (TyInt, SInt i)
  EBinOp op l r ->
    let isNum = \case
          TyInt -> True
     in do
          l'@(t1, _) <- checkExpr l
          r'@(t2, _) <- checkExpr r
          let sexpr = SBinOp op l' r'
              assertTypeEq = unless (t1 == t2) $ throwError $ TypeError [t1] t2 (StmtExpr expr)
              checkNumeric = do
                unless (isNum t1) $ throwError $ TypeError [TyInt] t1 (StmtExpr expr)
                pure (t1, SBinOp op l' r')
          case op of
            Plus -> case (t1, t2) of -- use this syntax when more types
              (TyInt, TyInt) -> pure (TyInt, sexpr)
            Minus -> assertTypeEq >> checkNumeric
            Multiply -> assertTypeEq >> checkNumeric
            Divide -> assertTypeEq >> checkNumeric
            Modulus -> assertTypeEq >> checkNumeric

checkStatement :: Statement -> Semant SStatement
checkStatement stmt = case stmt of
  StmtExpr e -> SStmtExpr <$> checkExpr e
  StmtBlock es -> do
    let flattened = flatten es
    SStmtBlock <$> mapM checkStatement flattened
    where
      flatten [] = []
      flatten (StmtBlock s : xs) = flatten (s ++ xs)
      flatten (s : xs) = s : flatten xs

checkProgram :: Program -> Either SemantError SProgram
checkProgram program = evalState (runExceptT (checkProgram' program)) []
  where
    checkProgram' :: Program -> Semant SProgram
    checkProgram' (Program stmts) = do
      mapM checkStatement stmts
