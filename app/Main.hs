module Main (main) where

import Control.Applicative
import Data.Char
import System.Environment
import Text.Earley

import Lib

main :: IO ()
main = do
  x : _ <- getArgs
  print $ fullParses (parser parseHadron) x
