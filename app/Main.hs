module Main where

import           Lib
import           System.Environment

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = (fst . head . reads) <$> rawArgs
  if null args
     then runAll
     else run args
