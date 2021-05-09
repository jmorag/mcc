{-# LANGUAGE TypeApplications #-}
module Main ( main)  where

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
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Control.Monad
import           Control.Exception

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
goldenTests = testGroup "all" <$> sequence [passing, failing, parsing]

parsing :: IO TestTree
parsing = do
  files <- concat <$> mapM (findByExtension [".mc"]) ["tests/pass", "tests/fail"]
  fmap (testGroup "parsing") $ forM files $ \file -> do
    input      <- T.readFile file
    let combinator = runParser programP file input
    generator  <- try @IOError . evaluate . parse . alexScanTokens $ cs input
    pure . testCase file $ case (combinator, generator) of
      (Right ast, Right ast') -> assertEqual file ast ast'
      (Left  _  , Left _    ) -> pure ()
      _                       -> assertFailure file

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
