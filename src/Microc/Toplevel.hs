module Microc.Toplevel where

import           LLVM.AST
import           LLVM.Pretty

import           Data.String.Conversions
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T

import           System.IO
import           System.Directory
import           System.Process
import           System.Posix.Temp

import           Control.Exception              ( bracket )

-- | Generate an executable at the given filepath from an llvm module
compile :: Module -> FilePath -> IO ()
compile llvmModule outfile =
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      -- create temporary file for "output.ll"
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      let runtime = "../src/runtime.c"
      -- write the llvmModule to a file
      T.hPutStrLn llvmHandle (cs $ ppllvm llvmModule)
      hClose llvmHandle
      -- link the runtime with the assembly
      callProcess
        "clang"
        ["-Wno-override-module", "-lm", llvm, runtime, "-o", "../" <> outfile]

-- | Compile and llvm module and read the results of executing it
run :: Module -> IO Text
run llvmModule = do
  compile llvmModule "./a.out"
  result <- cs <$> readProcess "./a.out" [] []
  removePathForcibly "./a.out"
  return result
