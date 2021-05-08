module Microc.Toplevel ( compile, run ) where

import           LLVM.AST

import           Data.String.Conversions
import           Data.Text                      ( Text )
import qualified LLVM.Module                   as LLVM
import           LLVM.Context
import           LLVM.Analysis

import           System.Directory
import           System.Process
import           System.Posix.Temp

import           Control.Exception

-- | Generate an executable at the given filepath from an llvm module
compile :: Module -> FilePath -> IO ()
compile llvmModule outfile =
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      let llvm = "output.ll"
          runtime = "../src/runtime.c"
      -- write the llvmModule to a file
      withContext $ \ctx -> LLVM.withModuleFromAST
        ctx
        llvmModule
        (\modl -> verify modl >> LLVM.writeBitcodeToFile (LLVM.File llvm) modl)
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
