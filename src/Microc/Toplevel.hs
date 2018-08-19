module Microc.Toplevel where

import LLVM.AST
import LLVM.Pretty

import           Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO
import System.Directory
import System.Process
import System.Posix.Temp

import Control.Exception (bracket)

-- | Generate an executable at the given filepath from an llvm module
compile :: Module -> FilePath -> IO ()
compile llvmModule outfile =
  bracket (mkdtemp "build") removeDirectoryRecursive $ \buildDir ->
    withCurrentDirectory buildDir $ do
      -- create temporary files for "output.ll", "output.s", and "runtime.o"
      (llvm, llvmHandle) <- mkstemps "output" ".ll"
      assembly           <- fst <$> mkstemps "output" ".s"
      runtime            <- fst <$> mkstemps "runtime" ".o"
      -- write the llvmModule to a file
      T.hPutStrLn llvmHandle (cs $ ppllvm llvmModule) >> hClose llvmHandle
      -- call the llc executable on the llvm to turn it into assembly
      call        "llc"      [llvm, "-o", assembly]
      -- generate the runtime object file
      call        "clang"    ["-c", "../src/runtime.c", "-o", runtime]
      -- link the runtime with the assembly
      call        "clang"    [assembly, runtime, "-o", "../" <> outfile]

-- | Call a command and print diagnostic information if it does anything interesting
call :: FilePath -> [String] -> IO ()
call command args = do
  (_, Just stdOut, Just stdErr, _) <- createProcess (proc command args)
    { std_out = CreatePipe
    , std_err = CreatePipe
    }
  out <- T.hGetContents stdOut
  err <- T.hGetContents stdErr
  if T.null out && T.null err then return () else do
    T.putStrLn . T.unwords $ [">", cs command] ++ map cs args
    T.putStrLn out
    T.putStrLn err

run :: Module -> IO ()
run llvmModule = bracket (fst <$> mkstemps "a" ".out") removeFile $ \temp -> do
  compile llvmModule temp
  (_, Just result, _, _) <- createProcess (shell ("./" <> temp))
    { std_out = CreatePipe }
  T.hGetContents result >>= T.putStr
