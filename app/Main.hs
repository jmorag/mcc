module Main where

import           Microc                  hiding ( Parser )

import           Options.Applicative
import           LLVM.Pretty
import           Data.String.Conversions
import qualified Data.Text.IO                  as T

import           Text.Pretty.Simple
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Text.Megaparsec                ( parseErrorPretty' )

data Action = Ast | Sast | LLVM | Compile FilePath | Run
data Options = Options { action :: Action, infile :: FilePath }

actionP :: Parser Action
actionP =
  flag' Ast (long "ast" <> short 'a')
    <|> flag' Sast    (long "sast" <> short 's')
    <|> flag' LLVM    (long "llvm" <> short 'l')
    <|> flag' Compile (long "compile" <> short 'c')
    <*> strOption (short 'o' <> value "a.out")
  -- running the file to see the expected output is default
    <|> pure Run

optionsP :: Parser Options
optionsP =
  Options <$> actionP <*> strArgument (help "input file" <> metavar "FILE")

main :: IO ()
main = runOpts =<< execParser (optionsP `withInfo` "Compile stuff")
  where withInfo opts desc = info (helper <*> opts) $ progDesc desc

runOpts :: Options -> IO ()
runOpts (Options action infile) = do
  program <- T.readFile infile
  let parseTree = runParser programP (cs infile) program
  case parseTree of
    Left  e   -> putStrLn $ parseErrorPretty' program e
    Right ast -> case action of
      Ast -> putDoc $ pretty ast
      _   -> case checkProgram ast of
        Left err -> putDoc $ pretty err
        Right sast ->
          let llvm = codegenProgram sast
          in  case action of
                Sast            -> pPrint sast
                LLVM            -> T.putStrLn . cs . ppllvm $ llvm
                Compile outfile -> compile llvm outfile
                Run             -> run llvm >>= T.putStr
                Ast             -> error "unreachable"
