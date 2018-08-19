module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden
import System.FilePath (takeBaseName, replaceExtension)

import Microc

import           Data.String.Conversions
import qualified Data.Text.IO as T
import           Data.ByteString.Lazy (ByteString)

import System.IO.Silently

-- | Given a microc file, attempt to compile and execute it and write the
-- results to a new file to be compared with what should be the correct output
runFile :: FilePath -> IO ByteString
runFile infile = do
  program <- T.readFile infile
  let parseTree = runParser programP (cs infile) program
  case parseTree of
    Left _ -> redirect $ parseTest' programP program
    Right ast -> case checkProgram ast of
      Left err -> redirect $ T.putStrLn err
      Right sast -> do
        let llvmModule = codegenProgram sast
        redirect $ run llvmModule
  where
    redirect action = cs <$> capture_ action

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  passingFiles <- findByExtension [".mc"] "tests/pass"
  failingFiles <- findByExtension [".mc"] "tests/fail"
  return $ testGroup "microc compiler output tests" $
    [ goldenVsString (takeBaseName mcFile) outFile (runFile mcFile) 
      | mcFile <- passingFiles, let outFile = replaceExtension mcFile ".out" ]
      ++
    [ goldenVsString (takeBaseName mcFile) outFile (runFile mcFile) 
      | mcFile <- failingFiles, let outFile = replaceExtension mcFile ".err" ]
