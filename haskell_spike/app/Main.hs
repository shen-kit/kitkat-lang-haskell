{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.Lazy.IO qualified as T
import LLVM.Pretty (ppllvm)
import Lexer.Lexer (lexer)
import Midend.Codegen (generateLLVM)
import Parser.Parser (pProgram)
import Parser.SemantParser (checkProgram)
import Parser.SemantParserTypes (SProgram)
import System.IO (readFile)
import Text.Megaparsec (parse)

-- textToSProgram :: Text -> Either () SProgram
-- textToSProgram txt = case parse lexer "" txt of
--   Left _ -> putStrLn "lexer err"
--   Right tokens -> case parse pProgram "" tokens of
--     Left _ -> putStrLn "parser err"
--     Right program -> case checkProgram program of
--       Left _ -> putStrLn "semant err"
--       Right sProgram -> sProgram
--
-- parseFile :: FilePath -> Either String SProgram
-- parseFile filepath = do
--   contents <- fromString $ readFile filepath
--   pure $ textToSProgram contents
--
-- compileFileToLLVM :: FilePath -> FilePath -> IO ()
-- compileFileToLLVM inFile outFile = do
--   parsedResult <- parseFile inFile
--   case parsedResult of
--     Left _ -> putStrLn "parse error"
--     Right sProgram -> do
--       let llvmModule = generateLLVM sProgram
--       let llvmText = ppllvm llvmModule
--       T.writeFile outFile llvmText

main :: IO ()
main = do
  putStr "outfile: "
  outFile <- getLine
  putStr "to compile: "
  inStr <- getLine
  let txt = fromString inStr
  case parse lexer "" txt of
    Left _ -> error "lexer error"
    Right tokens -> case parse pProgram "" tokens of
      Left _ -> error "parser error"
      Right ast -> case checkProgram ast of
        Left _ -> error "semant error"
        Right sast -> do
          let llvmModule = generateLLVM sast
          let llvmText = ppllvm llvmModule
          T.writeFile outFile llvmText
