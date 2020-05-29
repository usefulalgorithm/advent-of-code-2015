module Days.Day25 (day25) where

import           Days.ReadPuzzle

day25 :: IO ()
day25 = do
  input <- readPuzzle 25
  putStr "First: "
  print input
