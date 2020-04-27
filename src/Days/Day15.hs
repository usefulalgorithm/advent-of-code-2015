module Days.Day15 (day15) where

import           Data.Char
import           Data.List
import           Days.ReadPuzzle

day15 :: IO ()
day15 = do
  input <- readPuzzle 15
  putStr "First: "
  first input
  putStr "Second: "
  second input

second :: String -> IO ()
second input = do
  go True $ parseIngredients input

first :: String -> IO ()
first input = do
  go False $ parseIngredients input

go :: Bool -> [[Int]] -> IO ()
go doFilter ingredients =
  print $ maximum $ map (\c -> product $ (max 0 . dot c) <$> scored) $ filter f combinations
    where combinations = makeCombinations (length ingredients) 100
          attributes = transpose $ ingredients
          scored = init attributes
          calories = last attributes
          f = case doFilter of
                False -> const True
                True  -> \c -> 500 == sum (zipWith (*) calories c)

dot :: [Int] -> [Int] -> Int
dot a b = sum $ zipWith (*) a b

makeCombinations :: Int -> Int -> [[Int]]
makeCombinations 1 l = [[l]]
makeCombinations n l = [x:xs | x <- [0..l], xs <- makeCombinations (n-1) (l-x)]

parseIngredients :: String -> [[Int]]
parseIngredients = map (map read . filter (not . null) . map (filter (\x -> isNumber x || x == '-')) . words) . lines
