{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import Ast
import Sast
import Semant
import Codegen

import Data.List (isSuffixOf)
import Text.Megaparsec (runParser, parseTest')
import System.Directory
import Control.Monad
import Options.Applicative
import Data.Semigroup ((<>))

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

import LLVM.Pretty
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import qualified LLVM.IRBuilder.Instruction as L

data Action = Ast | Sast | LLVM | Compile FilePath
data Options = Options Action FilePath

actionP :: Parser Action
actionP = flag' Ast (long "ast" <> short 'a')
  <|> flag' Sast (long "sast" <> short 's')
  <|> flag' LLVM (long "llvm" <> short 'l')
  <|> flag' Compile (long "compile" <> short 'c') 
            <*> strOption (short 'o' <> value "a.out")
  -- Compile is default, so in the absence of a flag, return this
  <|> Compile <$> strOption (short 'o' <> value "a.out")
              
optionsP :: Parser Options
optionsP = Options <$> actionP <*> strArgument (help "input file" <> metavar "FILE")
              
main :: IO ()
main = run =<< execParser (optionsP `withInfo` "Compile stuff")
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc

run :: Options -> IO ()
run (Options action infile) = do
  program <- readFile infile
  let parseTree = runParser programP infile program
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
            LLVM -> T.putStrLn . ppllvm $ codegenProgram sast
            Compile outfile -> undefined

testInput :: Action -> String -> IO ()
testInput a input = go input where
  go input = case runParser programP "STDIN" input of
    Left err -> print err
    Right ast ->
      case a of 
        Ast -> print ast
        _ -> 
          case checkProgram ast of
            Left err -> print err
            Right sast -> 
              case a of
                Sast -> print sast
                LLVM -> T.putStrLn . ppllvm $ codegenProgram sast
                Compile outfile -> undefined
      


test :: Action -> IO ()
test action = do
  passing <- filter (isSuffixOf ".mc") <$> listDirectory "tests/pass"
  forM_ passing $ \infile -> withCurrentDirectory "tests/pass" $ do
    program <- readFile infile
    let parseTree = runParser programP infile program
    case parseTree of
      Left _ -> parseTest' programP program
      Right ast ->
        case action of 
          Ast -> return ()
          Sast -> case checkProgram ast of
                    Left err -> do putStrLn program 
                                   putStrLn err 
                                   putStrLn (replicate 150 '-')
                    Right sast -> return ()
          LLVM -> undefined
          Compile outfile -> undefined
  failing <- filter (isSuffixOf ".mc") <$> listDirectory "tests/fail"
  forM_ failing $ \infile -> withCurrentDirectory "tests/fail" $ do
    program <- readFile infile
    let parseTree = runParser programP infile program
    case parseTree of
      Left _ -> parseTest' programP program
      Right ast ->
        case action of 
          Ast -> return ()
          Sast -> case checkProgram ast of
                    Left err -> return ()
                    Right sast -> do putStrLn program
                                     print sast
                                     putStrLn (replicate 150 '-')
          LLVM -> undefined
          Compile outfile -> undefined

