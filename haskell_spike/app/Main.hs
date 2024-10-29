{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (IsString (fromString))
import Lexer.Lexer (lexer)
import Parser.Parser
import Parser.ParserTypes
import Parser.SemantParser
import Text.Megaparsec (parse)

main :: IO ()
main = do
  putStr "Enter text to parse: "
  inStr <- getLine
  let input = fromString inStr
  case parse lexer "" input of
    Left _ -> putStrLn "lexer err"
    Right tokens -> case parse pProgram "" tokens of
      Left _ -> putStrLn "parser err"
      Right expr -> print expr
