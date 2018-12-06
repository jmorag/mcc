module Main where

import           Test.Tasty                     ( defaultMain
                                                , TestTree
                                                , testGroup
                                                )
import           Test.Tasty.Golden
import           System.FilePath                ( takeBaseName
                                                , replaceExtension
                                                )

import           Microc

import           Data.String.Conversions
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Text.Megaparsec                ( errorBundlePretty )

-- | Given a microc file, attempt to compile and execute it and read the
-- results to be compared with what should be the correct output
runFile :: FilePath -> IO Text
runFile infile = do
  program <- T.readFile infile
  let parseTree = runParser programP (cs infile) program
  case parseTree of
    Left  e   -> return . cs $ errorBundlePretty e
    Right ast -> case checkProgram ast of
      Left err ->
        return . renderStrict $ layoutPretty defaultLayoutOptions (pretty err)
      Right sast -> run (codegenProgram sast)

main :: IO ()
main = defaultMain =<< goldenTests

-- | All of the test cases
-- General structure taken from 
-- https://ro-che.info/articles/2017-12-04-golden-tests
goldenTests :: IO TestTree
goldenTests = testGroup "all" <$> sequence [passing, failing]

passing :: IO TestTree
passing = do
  mcFiles <- findByExtension [".mc"] "tests/pass"
  return $ testGroup
    "pass"
    [ goldenVsString (takeBaseName mcFile) outfile (cs <$> runFile mcFile)
    | mcFile <- mcFiles
    , let outfile = replaceExtension mcFile ".golden"
    ]

failing :: IO TestTree
failing = do
  mcFiles <- findByExtension [".mc"] "tests/fail"
  return $ testGroup
    "fail"
    [ goldenVsString (takeBaseName mcFile) outfile (cs <$> runFile mcFile)
    | mcFile <- mcFiles
    , let outfile = replaceExtension mcFile ".golden"
    ]
