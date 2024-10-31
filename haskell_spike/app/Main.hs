{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (IsString (fromString))
import Lexer.Lexer (lexer)
import Midend.Codegen (generateLLVM)
import Parser.Parser (pProgram)
import Parser.SemantParser (checkProgram)
import Text.Megaparsec (parse)
import Linker as Linker
import System.IO (readFile)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let infile = args!!0
  let outFile = args!!1
  -- infile <- putStr "file to compile: " *> getLine
  contentsStr <- readFile infile
  let txt = fromString contentsStr
  -- putStr "outfile: "
  -- outFile <- getLine
  case parse lexer "" txt of
    Left _ -> error "lexer error"
    Right tokens -> case parse pProgram "" tokens of
      Left _ -> error "parser error"
      Right ast -> case checkProgram ast of
        Left _ -> error "semant error"
        Right sast -> do
          let llvmModule = generateLLVM sast
          Linker.compile llvmModule outFile
          putStrLn $ "wrote to " ++ outFile
