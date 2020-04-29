module Days.Day17 (day17) where

import           Data.List
import           Days.ReadPuzzle

day17 :: IO ()
day17 = do
  input <- readPuzzle 17
  putStr "First: "
  first input
  putStr "Second: "
  second input

first :: String -> IO ()
first input = do
  print . length $ mkContainers input

second :: String -> IO ()
second input = do
  print . length . head . group . sort . map length $ mkContainers input

mkContainers :: String -> [[Int]]
mkContainers input = filter ((==) 150 . sum) . subsequences . map read $ lines input
