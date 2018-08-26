{-# LANGUAGE OverloadedStrings #-}
module Main where

import Microc hiding (Parser)

import Options.Applicative
import LLVM.Pretty
import Data.String.Conversions
import qualified Data.Text.IO as T

import Text.Pretty.Simple

data Action = Ast | Sast | LLVM | Compile FilePath | Run
data Options = Options { action :: Action, infile :: FilePath }

actionP :: Parser Action
actionP = flag' Ast (long "ast" <> short 'a')
  <|> flag' Sast (long "sast" <> short 's')
  <|> flag' LLVM (long "llvm" <> short 'l')
  <|> flag' Compile (long "compile" <> short 'c') 
            <*> strOption (short 'o' <> value "a.out")
  -- running the file to see the expected output is default
  <|> pure Run
              
optionsP :: Parser Options
optionsP = Options 
  <$> actionP 
  <*> strArgument (help "input file" <> metavar "FILE")
              
main :: IO ()
main = runOpts =<< execParser (optionsP `withInfo` "Compile stuff")
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc

runOpts :: Options -> IO ()
runOpts (Options action infile) = do 
  program <- T.readFile infile
  let parseTree = runParser programP (show infile) program
  case parseTree of
    Left _ -> parseTest' programP program
    Right ast ->
      case action of 
        Ast -> pPrint ast
        _ -> 
          case checkProgram ast of
          Left err -> T.putStrLn err
          Right sast -> 
            case action of
            Sast -> pPrint sast
            LLVM -> T.putStrLn . cs . ppllvm $ codegenProgram sast
            Compile outfile -> do
              let llvm = codegenProgram sast
              compile llvm outfile
            Run -> run (codegenProgram sast)
            Ast -> error "unreachable"
