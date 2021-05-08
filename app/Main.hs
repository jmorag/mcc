 {-# LANGUAGE StrictData #-}
module Main(main) where

import           Microc                  hiding ( Parser )

import           Options.Applicative
import           LLVM.Pretty
import           Data.String.Conversions
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Text.Pretty.Simple
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

data Action = Ast | Sast | LLVM | Compile FilePath | Run
data ParserType = Combinator | Generator
data Options = Options { _action :: Action, _infile :: FilePath, _parser :: ParserType }

actionP :: Parser Action
actionP = flag' Ast (long "ast" <> short 'a' <> help "Pretty print the ast")
    <|> flag' Sast (long "sast" <> short 's' <> help "Pretty print the sast")
    <|> flag' LLVM (long "llvm" <> short 'l' <> help "Pretty print the generated llvm")
    <|> flag' Compile (long "compile" <> short 'c' <> help "Compile to an executable")
    <*> strOption (short 'o' <> value "a.out" <> metavar "FILE")
  -- running the file to see the expected output is default
    <|> pure Run

parserP :: Parser ParserType
parserP =
  flag'
      Combinator
      (long "combinator" <> help "Use the megaparsec parser implementation (default).")
    <|> flag'
          Generator
          (long "generator" <> short 'g' <> help "Use alex and happy to parse.")
    <|> pure Combinator -- default to megaparsec

optionsP :: Parser Options
optionsP =
  Options
    <$> actionP
    <*> strArgument (help "Source file" <> metavar "FILE")
    <*> parserP

main :: IO ()
main = runOpts =<< execParser (optionsP `withInfo` infoString)
 where
  withInfo opts desc = info (helper <*> opts) $ progDesc desc
  infoString
    = "Run the mcc compiler on the given file. \
       \Passing no flags will compile the file, execute it, and print the output."

runOpts :: Options -> IO ()
runOpts (Options action infile ptype) = do
  program <- T.readFile infile
  let parseTree = case ptype of
        Combinator -> runParser programP infile program
        Generator  -> Right $ parse . alexScanTokens $ T.unpack program
  case parseTree of
    Left  err -> putStrLn $ errorBundlePretty err
    Right ast -> case action of
      Ast -> putDoc $ pretty ast <> "\n"
      _   -> case checkProgram ast of
        Left err -> putDoc $ pretty err <> "\n"
        Right sast ->
          let llvm = codegenProgram sast
          in  case action of
                Sast            -> pPrint sast
                LLVM            -> T.putStrLn . cs . ppllvm $ llvm
                Compile outfile -> compile llvm outfile
                Run             -> run llvm >>= T.putStr
                Ast             -> error "unreachable"
