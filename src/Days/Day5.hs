module Days.Day5 (day5) where

import           Data.List
import           Data.String.Utils
import           Days.ReadPuzzle

day5 :: IO ()
day5 = do
  input <- readPuzzle 5
  putStr "First: "
  go input [niceOne, niceTwo, niceThree]
  putStr "Second: "
  go input [niceFour, niceFive]
    where niceOne xs = (length (filter isVowel xs)) > 2
          niceTwo xs = any (\c -> isInfixOf (replicate 2 c) xs) ['a'..'z']
          niceThree xs = not $ any (\p -> isInfixOf p xs) ["ab", "cd", "pq", "xy"]
          isVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'
          niceFour xs = any (\(x, y) -> isInfixOf (reverse (take 2 (reverse x))) y) $ map (\n -> splitAt n xs) [2..length xs]
          niceFive xs = any (\(x, y) -> isInfixOf [x,y,x] xs) $ [(x,y) | x <- ['a'..'z'], y <- ['a'..'z']]

go :: String -> [(String -> Bool)]-> IO ()
go s funcs = do
  print $ length $ filter (isNice . strip) $ lines s
    where isNice s' = and $ map ($ s') funcs
