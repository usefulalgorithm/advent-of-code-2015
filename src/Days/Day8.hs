module Days.Day8 (day8) where

import           Days.ReadPuzzle

day8 :: IO ()
day8 = do
  input <- readPuzzle 8
  putStr "First: "
  first input
  putStr "Second: "
  second input

countEscaped :: String -> Int
countEscaped "" = 2
countEscaped ('\\' : 'x' : a : b : xs) = 1 + countEscaped xs
countEscaped ('\\' : '"' : xs) = 1 + countEscaped xs
countEscaped ('\\' : '\\' :xs) = 1 + countEscaped xs
countEscaped (x:xs) = 1 + countEscaped xs

countEscapedString :: String -> Int
countEscapedString s
  | head s == '"' && last s == '"' = countEscaped s - 2
  | otherwise = error "bad string"

first :: String -> IO ()
first input = do
  print $ sum $ map (\s -> length s - countEscapedString s) $ lines input

second :: String -> IO ()
second input = do
  print $ sum $ map (\s -> 2 + countEncodedString s - length s) $ lines input

countEncodedString :: String -> Int
countEncodedString ('"':xs) = 2 + countEncodedString xs
countEncodedString ('\\':xs) = 2 + countEncodedString xs
countEncodedString (x:xs) = 1 + countEncodedString xs
countEncodedString _ = 0
