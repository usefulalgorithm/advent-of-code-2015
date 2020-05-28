module Days.Day23 (day23) where

import           Days.ReadPuzzle

day23 :: IO ()
day23 = do
  input <- readPuzzle 23
  putStr "First: "
  first input

first :: String -> IO ()
first input = do
  print input
