module Days.Day18 (day18) where

import           Data.List
import           Days.ReadPuzzle

day18 :: IO ()
day18 = do
  input <- readPuzzle 18
  putStr "First: "
  first input

first :: String -> IO ()
first input = do
  print input
