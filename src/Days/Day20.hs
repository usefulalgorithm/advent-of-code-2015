module Days.Day20 (day20) where

import           Days.ReadPuzzle

day20 :: IO ()
day20 = do
  input <- readPuzzle 20
  putStr "First: "
  first input

first :: String -> IO ()
first input = do
  print input
