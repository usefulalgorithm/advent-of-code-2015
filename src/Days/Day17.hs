module Days.Day17 (day17) where

import           Days.ReadPuzzle

day17 :: IO ()
day17 = do
  input <- readPuzzle 17
  putStr "First: "
  first input

first :: String -> IO ()
first input = do
  print input
