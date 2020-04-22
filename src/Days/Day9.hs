module Days.Day9 (day9) where

import           Data.List
import qualified Data.Map        as M
import           Days.ReadPuzzle

day9 :: IO ()
day9 = do
  input <- readPuzzle 9
  putStr "First: "
  go input minimum
  putStr "Second: "
  go input maximum

go :: String -> ([Int] -> Int) -> IO ()
go input cmp = do
  let (cities, distances) = parseDistances input
  print $ cmp $ (calcDistance distances) <$> permutations cities

calcDistance :: DistMap -> [String] -> Int
calcDistance distances route = sum $ legDistance <$> toPairs route
  where toPairs [a, b] = [(a, b)]
        toPairs (x:xs) = (x, head xs):(toPairs xs)
        toPairs _      = error "bad distances"
        legDistance p = case M.lookup p distances of
                          Just d  -> d
                          Nothing -> error "invalid pair"

type DistMap = M.Map (String, String) Int

parseDistances :: String -> ([String], DistMap)
parseDistances s = (cities, distances)
  where distances = M.fromList $ concat $ (parseLine . words) <$> lines s
        cities = nub $ fst <$> M.keys distances

parseLine :: [String] -> [((String, String), Int)]
parseLine [from, "to", to, "=", distance] = [((from, to), r), ((to, from), r)]
  where r = read distance
parseLine _                               = error "bad pattern"
