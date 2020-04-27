module Days.Day16 (day16) where

import           Days.ReadPuzzle

day16 :: IO ()
day16 = do
  input <- readPuzzle 16
  putStr "First: "
  first input

first :: String -> IO ()
first input = do
  print input
