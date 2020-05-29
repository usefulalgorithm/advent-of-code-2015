module Days.Day25 (day25) where

import           Data.Char
import           Data.List
import           Days.ReadPuzzle

day25 :: IO ()
day25 = do
  input <- readPuzzle 25
  putStr "Answer: "
  let [row, col] = parseCoordinate input
  print $ foldl' (\r _ -> next r) start [1..which (row, col) - 1]

start :: Integer
start = 20151125

multiplier :: Integer
multiplier = 252533

modulo :: Integer
modulo = 33554393

next :: Integer -> Integer
next n = (n * multiplier) `mod` modulo

which :: (Int, Int) -> Int
which (row, col) = sum [1..col] + sum (take (row - 1) [col..])

parseCoordinate :: String -> [Int]
parseCoordinate = map (read . init) . filter (isNumber . head) . words
