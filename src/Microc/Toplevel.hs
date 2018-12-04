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
      -- create temporary files for "output.ll", "output.s", and "runtime.o"
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      assembly           <- fst <$> mkstemps "output" ".s"
      runtime            <- fst <$> mkstemps "runtime" ".o"
      -- write the llvmModule to a file
      T.hPutStrLn llvmHandle (cs $ ppllvm llvmModule) >> hClose llvmHandle
      -- call the llc executable on the llvm to turn it into assembly
      callProcess "llc"   [llvm, "-o", assembly]
      -- generate the runtime object file
      callProcess "clang" ["-c", "../src/runtime.c", "-o", runtime]
      -- link the runtime with the assembly
      callProcess "clang" [assembly, runtime, "-o", "../" <> outfile]

-- | Compile and llvm module and read the results of executing it
run :: Module -> IO Text
run llvmModule =
  bracket (fst <$> mkstemps "a" ".out") removePathForcibly $ \temp -> do
    compile llvmModule temp
    cs <$> readProcess ("./" <> temp) [] []
