module Days.Day22  where

import           Days.ReadPuzzle

day22 :: IO ()
day22 = do
  input <- readPuzzle 22
  putStr "First: "
  first input

first :: String -> IO ()
first input = do
  print input
