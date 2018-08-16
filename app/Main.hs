{-# LANGUAGE OverloadedStrings #-}
module Main where

import Microc hiding (Parser)

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

import LLVM.Pretty

import           Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)

data Action = Ast | Sast | LLVM | Compile FilePath
data Options = Options { action :: Action, infile :: FilePath, llc :: FilePath }

actionP :: Parser Action
actionP = flag' Ast (long "ast" <> short 'a')
  <|> flag' Sast (long "sast" <> short 's')
  <|> flag' LLVM (long "llvm" <> short 'l')
  -- Compile is default, so in the absence of a flag, return this
  <|> Compile <$> strOption (short 'o' <> value "a.out")
  <|> flag' Compile (long "compile" <> short 'c') 
            <*> strOption (short 'o' <> value "a.out")
              
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
  program <- T.readFile infile
  let parseTree = runParser programP (show infile) program
  case parseTree of
    Left _ -> parseTest' programP program
    Right ast ->
      case action of 
        Ast -> print ast
        _ -> 
          case checkProgram ast of
          Left err -> T.putStrLn err
          Right sast -> 
            case action of
            Sast -> print sast
            LLVM -> T.putStrLn . cs . ppllvm $ codegenProgram sast
            Compile outfile -> do
              let llvm = codegenProgram sast
              compile llvm outfile
            Ast -> error "unreachable"
