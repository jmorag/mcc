module Microc.Toplevel where

import Microc.Ast
import Microc.Sast
import Microc.Parser
import Microc.Semant
import Microc.Codegen

import LLVM.AST
import LLVM.Pretty

import           Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)

import System.IO
import System.Exit
import System.Directory
import System.Process
import System.Posix.Temp

-- | Generate an executable at the given filepath from an llvm module
compile :: Module -> FilePath -> IO ()
compile llvmModule outfile = do
  buildDir <- mkdtemp "_build"
  withCurrentDirectory buildDir $ do
    -- create temporary files for "output.ll", "output.s", and "runtime.o"
    (llvm    , llvmHandle) <- mkstemps "output" ".ll"
    (assembly, _         ) <- mkstemps "output" ".s"
    (runtime , _         ) <- mkstemps "runtime" ".o"
    -- write the llvmModule to a file
    T.hPutStrLn llvmHandle (cs $ ppllvm llvmModule) >> hClose llvmHandle
    -- call the llc executable on the llvm to turn it into assembly
    call        "llc"      [llvm, "-o", assembly]
    -- generate the runtime object file
    call        "clang"    ["-c", "../src/runtime.c", "-o", runtime]
    -- link the runtime with the assembly
    call        "clang"    [assembly, runtime, "-o", "../" <> outfile]
  -- clean up the build directory
  removeDirectoryRecursive buildDir

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
run llvmModule = do
  (temp, _) <- mkstemps "a" ".out"
  compile llvmModule temp
  (_, Just result, _, _) <- createProcess (shell ("./" <> temp)) { std_out = CreatePipe }
  T.hGetContents result >>= T.putStr
  removeFile temp
