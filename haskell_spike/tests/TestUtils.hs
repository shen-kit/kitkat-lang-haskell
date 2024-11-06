module TestUtils where

import Test.HUnit
import Text.Megaparsec
import Data.Either
import Data.Text (Text)
import Data.Void (Void)

-- testEq <expected> <parser> <parser_input>
-- checks for equality from running a parser
testEq :: (Show b, Eq a, Eq b) => Parsec Void a b -> a -> b -> Test
testEq p input expected = case parse p "" input of
  Left err -> TestCase $ assertFailure $ "parser failed to parse input"
  Right res -> expected ~=? res

-- checks that a parser fails when given an input
testFail :: (Eq a, Eq b, Show b) => Parsec Void a b -> a -> Test
testFail p input = TestCase $ assertBool "" (isLeft $ parse p "" input)
