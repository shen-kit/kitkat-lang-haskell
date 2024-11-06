{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import Test.HUnit hiding (State)
import Text.Megaparsec hiding (State)
import Data.Either
import Data.Text (Text)
import Data.Void (Void)

import Parser.ParserTypes (Type (..))
import Parser.SemantParserTypes
import Control.Monad.Except
import Control.Monad.State
import Data.Map as M

-- =======================================================
-- =                    PARSER TESTS                     =
-- =======================================================

-- checks for equality from running a parser
testParseEq :: (Show a, Show b, Eq a, Eq b) => Parsec Void a b -> a -> b -> Test
testParseEq p input expected = case parse p "" input of
  Left err -> TestCase $ assertFailure $ "parser failed to parse input: " ++ show input
  Right res -> expected ~=? res

-- checks that a parser fails when given an input
testParseFail :: (Eq a, Eq b, Show b) => Parsec Void a b -> a -> Test
testParseFail p input = TestCase $ assertBool "" (isLeft $ parse p "" input)

-- ======================================================
-- =                SEMANT PARSER TESTS                 =
-- ======================================================

-- initialise with variable myVar of type int (to test assignment/retrieval)
initialState :: SAst
initialState = SAst {body = [], vars = M.fromList [("myVar", TyInt)]}

testSemantEq :: (Eq c, Show c) => (a -> ExceptT String (State SAst) c) -> a -> c -> Test
testSemantEq f input expected = evalState (runExceptT (f input)) initialState ~=? Right expected
