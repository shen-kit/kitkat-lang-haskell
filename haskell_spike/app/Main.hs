{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (IsString (fromString))
import Lexer.Lexer (lexer)
import Lexer.TokenTypes (Token)
import Midend.Codegen (generateLLVM)
import Parser.Parser (parseProgram)
import Parser.ParserTypes (Ast)
import Parser.SemantParser (checkProgram)
import Parser.SemantParserTypes (SAst)
import Text.Megaparsec (parse, errorBundlePretty)
import Linker as Linker
import System.Environment (getArgs)
import Control.Monad (unless)
import System.Exit (die) -- show error msg and exit

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 2) $ die "Error: Use this program by running `kitkat <infile> <outfile>`"
  let [infile, outfile] = args
  putStr "select 1-2:\n1. print tokens\n2. print AST\n3. print SAST\n4. compile file\n: "
  choice <- getLine
  case choice of
    "1" -> getTokens infile >>= print
    "2" -> getAST infile >>= print
    "3" -> getSAST infile >>= print
    "4" -> compileFile infile outfile
    _ -> putStrLn "invalid option selected. Exiting..."
  where

    getTokens :: FilePath -> IO [Token]
    getTokens infile = do
      contentsStr <- readFile infile
      let txt = fromString contentsStr
      case parse lexer "" txt of
        Left err -> error $ errorBundlePretty err
        Right tokens -> pure tokens

    getAST :: FilePath -> IO Ast
    getAST infile = do
        tokens <- getTokens infile
        case parse parseProgram "" tokens of
          Left err -> error $ show err
          Right ast -> pure ast

    getSAST :: FilePath -> IO SAst
    getSAST infile = do
        ast <- getAST infile
        case checkProgram ast of
          Left err -> error $ show err
          Right sast -> pure sast

    compileFile :: FilePath -> FilePath -> IO ()
    compileFile infile outfile = do
      sast <- getSAST infile
      let llvmModule = generateLLVM sast
      Linker.compile llvmModule outfile
      putStrLn $ "wrote to " ++ outfile

