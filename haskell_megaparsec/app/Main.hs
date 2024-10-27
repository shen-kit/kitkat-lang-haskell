{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.IO (putStr)
import Lexer.Lexer
import Parser
import Text.Megaparsec (runParser)
import Types (Token (..))

inStr :: Text
inStr = "(1 + 2) * 3 % 4"

tokens :: [Token]
tokens = [TLParen, TInt 1, TBinOp "+", TInt 2, TRParen, TBinOp "*", TInt 3]

main :: IO ()
main = do
  inStr <- getLine
  let inText = fromString inStr
  case runParser Lexer.Lexer.lex [] inText of
    Left err -> putStrLn "lexer error"
    Right toks -> case parseTokens toks of
      Left err -> putStrLn "parser error"
      Right ast -> print ast
