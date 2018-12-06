module Main where

import           Microc                  hiding ( Parser )

import           Options.Applicative
import           LLVM.Pretty
import           Data.String.Conversions
import qualified Data.Text.IO                  as T

import           Text.Pretty.Simple
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Text.Megaparsec                ( errorBundlePretty )

data Action = Ast | Sast | LLVM | Compile FilePath | Run
data Options = Options { action :: Action, infile :: FilePath }

actionP :: Parser Action
actionP =
  flag' Ast (long "ast" <> short 'a' <> help "Pretty print the ast")
    <|> flag' Sast (long "sast" <> short 's' <> help "Pretty print the sast")
    <|> flag'
          LLVM
          (long "llvm" <> short 'l' <> help "Pretty print the generated llvm")
    <|> flag' Compile
              (long "compile" <> short 'c' <> help "Compile to an executable")
    <*> strOption (short 'o' <> value "a.out" <> metavar "FILE")
  -- running the file to see the expected output is default
    <|> pure Run

optionsP :: Parser Options
optionsP =
  Options <$> actionP <*> strArgument (help "Source file" <> metavar "FILE")

main :: IO ()
main = runOpts =<< execParser (optionsP `withInfo` infoString)
 where
  withInfo opts desc = info (helper <*> opts) $ progDesc desc
  infoString
    = "Run the mcc compiler on the given file. \
       \Passing no flags will compile the file, execute it, and print the output."

runOpts :: Options -> IO ()
runOpts (Options action infile) = do
  program <- T.readFile infile
  let parseTree = runParser programP (cs infile) program
  case parseTree of
    Left  e   -> putStrLn $ errorBundlePretty e
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
