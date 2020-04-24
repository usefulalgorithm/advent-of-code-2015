module Days.Day13 (day13) where

import           Data.List
import qualified Data.Map        as M
import           Days.ReadPuzzle

day13 :: IO ()
day13 = do
  input <- readPuzzle 13
  putStr "First: "
  first input
  putStr "Second: "
  second input

first :: String -> IO ()
first input = do
  let happiness = M.fromList $ (parseLine . words) <$> lines input
  print $ maxHappiness happiness

parseLine :: [String] -> ((String, String), Int)
parseLine (x:_:"gain":n:y) = ((x, init . last $ y), read n)
parseLine (x:_:"lose":n:y) = ((x, init . last $ y), negate . read $ n)
parseLine _                = error "bad sentence"

maxHappiness :: M.Map (String, String) Int -> Int
maxHappiness happiness = maximum $ sum <$> map getScore <$> createRev <$> pushBackHead <$> (permutations people)
  where people = nub . fmap fst . M.keys $ happiness
        pushBackHead []      = error "bad pattern"
        pushBackHead l@(x:_) = l ++ [x]
        getScore (x:xs@(y:_)) = case M.lookup (x, y) happiness of
                            Just n  -> n + getScore xs
                            Nothing -> error "invalid key"
        getScore _ = 0
        createRev xs = [xs, reverse xs]

second :: String -> IO ()
second input = do
  let happiness = M.fromList $ (parseLine . words) <$> lines input
  let people = nub . fmap fst . M.keys $ happiness
  let happiness' = M.union happiness $ M.fromList $ concat $ map (\p -> [(("Myself", p), 0), ((p, "Myself"), 0)]) people
  print $ maxHappiness happiness'
