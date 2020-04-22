module Days.Day11 (day11) where

import           Days.ReadPuzzle

day11 :: IO ()
day11 = do
  input <- init <$> readPuzzle 11
  putStr "First: "
  first input

first :: String -> IO ()
first input = do
  print input
