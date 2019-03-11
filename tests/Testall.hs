module Main where

import           Test.Tasty                     ( defaultMain
                                                , TestTree
                                                , testGroup
                                                )
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           System.FilePath                ( takeBaseName
                                                , replaceExtension
                                                )

import           Microc

import           Data.String.Conversions
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Control.Monad
import           Control.Exception

-- | Given a microc file, attempt to compile and execute it and read the
-- results to be compared with what should be the correct output
runFile :: FilePath -> IO Text
runFile infile = do
  program <- T.readFile infile
  let ast = parse . alexScanTokens $ unpack program
  case checkProgram ast of
    Left err ->
      return . renderStrict $ layoutPretty defaultLayoutOptions (pretty err)
    Right sast -> run (codegenProgram sast)

main :: IO ()
main = defaultMain =<< goldenTests

-- | All of the test cases
-- General structure taken from 
-- https://ro-che.info/articles/2017-12-04-golden-tests
goldenTests :: IO TestTree
goldenTests =
  testGroup "all" <$> sequence [passing, failing, parsing, parserFail]

parsing :: IO TestTree
parsing = do
  mcFiles <- findByExtension [".mc"] "tests/pass"
  testGroup "parse" <$> forM
    mcFiles
    (\mcFile -> do
      input <- T.readFile mcFile
      return $ testCase mcFile $ assertEqual
        mcFile
        (runParser programP mcFile input)
        (Right . parse . alexScanTokens $ cs input)
    )

parserFail :: IO TestTree
parserFail = do
  mcFiles <- findByExtension [".mc"] "tests/fail"
  testGroup "parse-fail" <$> forM
    mcFiles
    (\mcFile -> do
      input <- T.readFile mcFile
      case runParser programP mcFile input of
        Right ast -> return . testCase mcFile $ assertEqual
          mcFile
          ast
          (parse . alexScanTokens $ cs input)
        Left _err -> do
          failedParse <-
            try . evaluate . parse . alexScanTokens $ cs input :: IO
              (Either IOError Program)
          return . testCase mcFile $ assertBool mcFile (isLeft failedParse)
    )
 where
  isLeft = \case
    Left _ -> True
    _      -> False

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
