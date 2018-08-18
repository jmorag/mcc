module Testall where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, findByExtension)
import System.FilePath (takeBaseName, replaceExtension)

import Microc

import           Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)

import System.IO.Silently

-- | Given a microc file, attempt to compile and execute it and write the
-- results to a new file to be compared with what should be the correct output
runFile :: FilePath -> FilePath -> IO ()
runFile infile outfile = do
  program <- T.readFile infile
  let parseTree = runParser programP (show infile) program
  case parseTree of
    Left _ -> redirect $ parseTest' programP program
    Right ast -> case checkProgram ast of
      Left err -> redirect (T.putStrLn err)
      Right sast -> do
        let llvmModule = codegenProgram sast
        redirect (run llvmModule)
  where
    redirect action = cs <$> capture_ action >>= T.writeFile outfile

main :: IO ()
main = defaultMain =<< goldenTests
