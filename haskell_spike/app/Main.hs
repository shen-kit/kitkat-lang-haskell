{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (IsString (fromString))
import Data.Text.Lazy.IO as T
import LLVM.Pretty (ppllvm)
import Lexer.Lexer (lexer)
import Midend.Codegen (generateLLVM)
import Parser.Parser (pProgram)
import Parser.SemantParser (checkProgram)
import Parser.SemantParserTypes (SProgram)
import System.IO (readFile)
import Text.Megaparsec (parse)

textToSProgram :: Text -> Either Text SProgram
textToSProgram txt = case parse lexer "" txt of
  Left _ -> putStrLn "lexer err"
  Right tokens -> case parse pProgram "" tokens of
    Left _ -> putStrLn "parser err"
    Right program -> case checkProgram program of
      Left _ -> putStrLn "semant err"
      Right sProgram -> sProgram

parseFile :: FilePath -> Either String SProgram
parseFile filepath = do
  contents <- fromString $ readFile filepath
  pure $ textToSProgram contents

compileFileToLLVM :: FilePath -> FilePath -> IO ()
compileFileToLLVM inFile outFile = do
  parsedResult <- parseFile inFile
  case parsedResult of
    Left err -> putStrLn $ "parse error: " ++ err
    Right sProgram -> do
      let llvmModule = generateLLVM sProgram
      let llvmText = ppllvm llvmModule
      T.writeFile outFile llvmText

main :: IO ()
main = compileFileToLLVM "in.kitkat" "out.ll"
