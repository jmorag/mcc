module Testall where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile, findByExtension)
import System.FilePath (takeBaseName, replaceExtension)

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Given a microc file, attempt to compile and execute it and write the
-- results to a new file to be compared with what should be the correct output
runFile :: FilePath -> FilePath -> IO ()
runFile infile outfile = do
  program <- T.readFile infile
  let parseTree = runParser programP (show infile) program
  case parseTree of
    Left _ -> parseTest' programP program -- write this to the outfile
    Right ast -> case checkProgram ast of
      Left err -> return () -- to be completed

main :: IO ()
main = defaultMain =<< goldenTests
