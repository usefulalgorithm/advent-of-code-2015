module Days.Day10 (day10) where

import           Data.List
import           Days.ReadPuzzle

day10 :: IO ()
day10 = do
  input <- init <$> readPuzzle 10
  putStr "First: "
  run input 40
  putStr "Second: "
  run input 50

run :: String -> Int -> IO ()
run input n = do
  print $ length $ process n input
    where process n' s = iterate say s !! n'
          say s = concat $ sayGroup <$> group s
          sayGroup g = show (length g) ++ [head g]
