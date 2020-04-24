module Days.Day14 (day14) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.List
import           Data.Ord
import           Days.ReadPuzzle

day14 :: IO ()
day14 = do
  input <- readPuzzle 14
  putStr "First: "
  first input
  putStr "Second: "
  second input

first :: String -> IO ()
first input = do
  print . maximum $ simSecs reindeers 2503
    where reindeers = parseReindeer . words <$> lines input

simSecs :: [[Int]] -> Int -> [Int]
simSecs reindeers n = zipWith (*) (zipWith (+) runs running) $ head <$> reindeers
  where times = tail <$> reindeers
        sequences = last <$> takeWhile (\l -> sum l < n) <$> inits <$> cycle <$> times
        runs = (\s -> if null s then 0 else head s * (flip div 2 . succ . length) s) <$> sequences
        running = map (\(t, s) -> if null s then n else if head t == last s then 0 else n - sum s) (zip times sequences)

parseReindeer :: [String] -> [Int]
parseReindeer (_:_:_:s:_:_:t:xs) = [read s, read t, read (last . init $ xs)]
parseReindeer _                  = error "bad pattern"

second :: String -> IO ()
second input =
  print . maximum . elems $ runSTArray $ getScores reindeers 2503
    where reindeers = parseReindeer . words <$> lines input

getScores :: [[Int]] -> Int -> ST s (STArray s Int Int)
getScores reindeers n = do
  scores <- newArray (1, length reindeers) 0
  forM_ [1..n] $ \t -> do
    let locs = simSecs reindeers t
    let indexes = map snd . head . groupBy (\a b -> fst a == fst b) . sortBy (flip (comparing fst)) $ (zip locs [1..])
    forM_ indexes $ \idx -> do
      score <- readArray scores idx
      writeArray scores idx $ succ score
  return scores
