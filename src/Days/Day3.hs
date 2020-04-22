module Days.Day3 (day3) where

import qualified Data.Set        as S
import           Days.ReadPuzzle

day3 :: IO ()
day3 = do
  input <- readPuzzle 3
  putStr "First: "
  first input
  putStr "Second: "
  second input

first :: String -> IO ()
first s = do
  let start = (0,0)
  let set = S.singleton start
  print $ S.size $ foldl (\x p -> S.insert p x) set $ scanl nextPoint start s

nextPoint :: (Int, Int) -> Char -> (Int, Int)
nextPoint (x, y) c = case c of
                       '^' -> (x, y+1)
                       'v' -> (x, y-1)
                       '<' -> (x-1, y)
                       '>' -> (x+1, y)
                       _   -> (x, y)
evens :: [a] -> [a]
evens (x:xs) = x:odds xs
evens _      = []

odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _      = []

second :: String -> IO ()
second input = do
  let start = (0,0)
  let oddSet = getPoints start $ odds input
  let evenSet = getPoints start $ evens input
  print $ S.size $ S.union oddSet evenSet
    where getPoints start s = foldl (\x p -> S.insert p x) (S.singleton start) $ scanl nextPoint start s
