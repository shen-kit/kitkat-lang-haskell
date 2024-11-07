{-# LANGUAGE ImportQualifiedPost #-}

module Linker (compile) where

import Data.Text.IO qualified as T
import Data.Text.Lazy
import LLVM.AST
import LLVM.Pretty (ppllvm)
import System.Directory
import System.Process

-- generates an executable from a module at a given path
compile :: Module -> FilePath -> IO ()
compile llvmModule outfile = do
  -- createDirectoryIfMissing True "build"
  -- withCurrentDirectory "build" $ do
  let llvmText = ppllvm llvmModule
  T.writeFile (outfile ++ ".ll") (toStrict llvmText)
  callProcess "llc" [outfile ++ ".ll", "-o", outfile ++ ".s"]
  callProcess "gcc" [outfile ++ ".s", "-o", outfile, "-lc"]
