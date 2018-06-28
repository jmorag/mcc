module Main where

import Parser
import Ast
import Text.Megaparsec
import System.Directory

main :: IO ()
main = do
  passingTests <- listDirectory "tests/pass"
  forM_ passingTests $ \file -> do
    contents <- readFile file
    parseTree <- runParser programP contents file
    case parseTree of
      Left err -> print err
      Right success -> print success
    
