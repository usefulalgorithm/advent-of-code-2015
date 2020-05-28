module Days.Day24 (day24) where

import           Days.ReadPuzzle

day24 :: IO ()
day24 = do
  input <- readPuzzle 23
  putStr "First: "
  print input
