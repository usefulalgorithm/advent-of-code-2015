module Main where

import           Lib
import           System.Environment

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = (fst . head . reads) <$> rawArgs
  if null args
     then run [1..25]
     else run args
