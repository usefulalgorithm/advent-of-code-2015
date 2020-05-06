module Days.Day21 (day21) where

import           Days.ReadPuzzle

day21 :: IO ()
day21 = do
  input <- readPuzzle 21
  putStr "First: "
  first input

first :: String -> IO ()
first input = do
  print input
