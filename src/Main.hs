{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Ast
import Sast
import Semant
import Codegen

import Prelude hiding (FilePath)
import Text.Megaparsec (runParser, parseTest')
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import LLVM.Pretty

import Turtle

data Action = Ast | Sast | LLVM | Compile FilePath
data Options = Options { action :: Action, infile :: FilePath, llc :: FilePath }

actionP :: Parser Action
actionP = flag' Ast (long "ast" <> short 'a')
  <|> flag' Sast (long "sast" <> short 's')
  <|> flag' LLVM (long "llvm" <> short 'l')
  <|> flag' Compile (long "compile" <> short 'c') 
            <*> strOption (short 'o' <> value "a.out")
  -- Compile is default, so in the absence of a flag, return this
  <|> Compile <$> strOption (short 'o' <> value "a.out")
              
optionsP :: Parser Options
optionsP = Options 
  <$> actionP 
  <*> strArgument (help "input file" <> metavar "FILE")
  <*> strOption (long "llc" <> value "/usr/local/opt/llvm/bin/llc")
              
main :: IO ()
main = run =<< execParser (optionsP `withInfo` "Compile stuff")
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc

run :: Options -> IO ()
run (Options action infile llc) = do 
  program <- T.unpack <$> readTextFile infile
  let parseTree = runParser programP (show infile) program
  case parseTree of
    Left _ -> parseTest' programP program
    Right ast ->
      case action of 
        Ast -> print ast
        _ -> 
          case checkProgram ast of
          Left err -> putStrLn err
          Right sast -> 
            case action of
            Sast -> print sast
            LLVM -> putStrLn . TL.unpack . ppllvm $ codegenProgram sast
            Compile outfile -> do
              let llvm = T.pack . TL.unpack . ppllvm $ codegenProgram sast
              sh $ compile llc llvm outfile
            Ast -> error "unreachable"

compile :: FilePath -> Text -> FilePath -> Shell ()
compile llc llvm outfile = do
  currentDir <- pwd
  buildDir <- mktempdir currentDir "_build"
  bitcode <- mktempfile buildDir "output.ll"
  liftIO $ writeTextFile bitcode llvm
  let encodeText = T.pack . encodeString
      runtimeCmd = "clang -c src/runtime.c -o " <> 
                   encodeText buildDir <> "/runtime.o"
      llcCmd  = encodeText llc <> " " <> 
                encodeText (buildDir </> bitcode) <> " -o " <> 
                encodeText buildDir <> "/output.s"
      linkCmd = "clang " <> encodeText buildDir <> "/output.s " <> 
                encodeText buildDir <> "/runtime.o -o " <> encodeText outfile
  void $ shells runtimeCmd empty
  void $ shells llcCmd empty
  void $ shells linkCmd empty
