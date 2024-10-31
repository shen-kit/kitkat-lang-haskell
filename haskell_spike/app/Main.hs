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

main :: IO ()
main = do
  -- args <- getArgs
  -- let infile = args!!0
  -- let outFile = args!!1
  putStr "infile: "
  infile <- getLine
  let outFile = "out"
  contentsStr <- readFile infile
  let txt = fromString contentsStr
  case parse lexer "" txt of
    Left _ -> error "lexer error"
    Right tokens -> case parse pProgram "" tokens of
      Left _ -> error "parser error"
      Right ast -> case checkProgram ast of
        Left _ -> error "semant error"
        Right sast -> do
          -- print $ sast
          let llvmModule = generateLLVM sast
          Linker.compile llvmModule outFile
          putStrLn $ "wrote to " ++ outFile
