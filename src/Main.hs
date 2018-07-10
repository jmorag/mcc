module Main where

import Parser
import Ast
import Sast
import Semant
import Data.List (isSuffixOf)
import Text.Megaparsec (runParser, parseTest')
import System.Directory
import Control.Monad
import Options.Applicative
import Data.Semigroup ((<>))


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
        Sast -> case checkProgram ast of
                  Left err -> putStrLn err
                  Right sast -> print sast
        LLVM -> undefined
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
          Ast -> print ast
          Sast -> case checkProgram ast of
                    Left err -> putStrLn err
                    Right sast -> print sast
          LLVM -> undefined
          Compile outfile -> undefined

