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
first input =
  foldl (\acc x -> if x == '(' then acc+1 else if x == ')' then acc-1 else acc) 0 input

second :: String -> Int
second input =
  fst $ head $ filter ((((>) :: Int -> Int -> Bool) 0). snd) $ zip [0..] $ scanl step 0 input
    where step acc x = if x == '('
                          then acc + 1
                          else if x == ')'
                          then acc - 1
                          else acc
