module Days.Day20 (day20) where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Days.ReadPuzzle

day20 :: IO ()
day20 = do
  input <- liftM (read . init) $ readPuzzle 20
  putStr "First: "
  first input
  putStr "Second: "
  second input

first :: Int -> IO ()
first input = do
  print . fromJust . findIndex (>= input) . map ((10 *) . sum . factors) $ [0..]

factors :: Int -> [Int]
factors n = nub . concat . map (\x -> [x, n `div` x] ) $ filter (\x -> n `mod` x == 0) [1.. ((floor :: Double -> Int) . sqrt . fromIntegral) n]

second :: Int -> IO ()
second input = do
  print . fromJust . findIndex (>= input) . map calc $ [0..]
    where calc n = (11 *) . sum . filter (> (n - 1) `div` 50) . factors $ n
