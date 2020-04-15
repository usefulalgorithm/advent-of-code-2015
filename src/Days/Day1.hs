module Days.Day1 (day1) where

import           Days.ReadPuzzle

day1 :: IO ()
day1 = do
  input <- readPuzzle 1
  putStr "First: "
  print $ first input
  putStr "Second: "
  print $ second input

first :: String -> Int
first x =
  foldl (\acc x -> if x == '(' then acc+1 else if x == ')' then acc-1 else acc) 0 x

second :: String -> Int
second x =
  fst $ head $ filter (\(x, y) -> y < 0) $ zip [0..] $ scanl (\acc x -> if x == '(' then acc+1 else if x == ')' then acc-1 else acc) 0 x
