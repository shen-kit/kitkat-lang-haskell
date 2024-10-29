module Parser.SemantParser where

import Control.Exception (TypeError (TypeError), throw)
import Control.Monad (when)
import Parser.Parser
import Parser.ParserTypes (BOp (..), Expr (..), Type (..))
import Parser.SemantParserTypes (SExpr, SExpr' (..))

-- converts expressions into semantic expressions using type inferencing
-- also performs type-checking on operations
checkExpr :: Expr -> SExpr
checkExpr expr = case expr of
  EInt i -> (TyInt, SInt i)
  EBinOp op l r -> do
    let l'@(t1, _) = checkExpr l
    let r'@(t2, _) = checkExpr r
    let sexpr = SBinOp op l' r'
    case op of
      Plus -> case (t1, t2) of
        (TyInt, TyInt) -> (TyInt, sexpr)
      Minus -> case (t1, t2) of
        (TyInt, TyInt) -> (TyInt, sexpr)
      Multiply -> case (t1, t2) of
        (TyInt, TyInt) -> (TyInt, sexpr)
      Divide -> case (t1, t2) of
        (TyInt, TyInt) -> (TyInt, sexpr)
      Modulus -> case (t1, t2) of
        (TyInt, TyInt) -> (TyInt, sexpr)
