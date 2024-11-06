{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit
import Text.Megaparsec
import Parser.Parser
import Parser.ParserTypes
import Lexer.TokenTypes


tests :: Test
tests = TestList []

main :: IO ()
main = runTestTTAndExit tests
