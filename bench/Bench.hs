module Main where

import           Microc
import           Data.String.Conversions
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           System.FilePath                ( takeBaseName
                                                , replaceExtension
                                                )
import           Criterion.Main
import           Test.Tasty.Golden
import           Control.Monad

megaparse input = case runParser programP "" input of
  Left _ -> error "We only like success"
  Right p -> p

happyparse = alexScanTokens . parse

main = do
  mcFiles <- findByExtension [".mc"] "tests/pass"
  defaultMain []
