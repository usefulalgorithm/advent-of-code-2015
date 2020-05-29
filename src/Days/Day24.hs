module Days.Day24 (day24) where

import           Control.Monad
import           Data.Function
import           Data.List
import           Days.ReadPuzzle

day24 :: IO ()
day24 = do
  input <- readPuzzle 24
  let packages = parseInput input
  putStr "First: "
  print $ getOptimalQE 3 packages
  putStr "Second: "
  print $ getOptimalQE 4 packages

getOptimalQE :: Int -> [Int] -> Integer
getOptimalQE n = minimum . map entanglement . smallestPacks . packup n . reverse

smallestPacks :: [[Int]] -> [[Int]]
smallestPacks = foldl go [[]]
  where
    go [[]] p = [p]
    go r p
      | length (head r) > length p = [p]
      | length (head r) == length p = p:r
      | otherwise = r

packup :: Int -> [Int] -> [[Int]]
packup n packages = go packages (sum packages `div` n) (length packages `div` n)
  where
    go :: [Int] -> Int -> Int -> [[Int]]
    go _ 0 _      = [[]]
    go [] _ _     = []
    go _ _ 0      = []
    go (x:xs) m l = (map (x:) $ go xs (m-x) (l-1)) ++ go xs m l

_bruteForce :: [Int] -> Integer
_bruteForce packages =
  head . sort . map entanglement . head . groupBy ((==) `on` length) . sortOn length . nub $ map head compartments
    where compartments = [[c1, c2, c3] | c1 <- getCompartment packages,
                                         c2 <- getCompartment packages,
                                         c3 <- getCompartment packages,
                                         c1 /= c2, c2 /= c3, c3 /= c1,
                                         length c1 <= length c2,
                                         length c2 <= length c3,
                                         c1 `isDisjoint` c2,
                                         c2 `isDisjoint` c3,
                                         c1 `isDisjoint` c3
                         ]

_bruteForce2 :: [Int] -> Integer
_bruteForce2 packages =
  head . sort . map entanglement . head . groupBy ((==) `on` length) . sortOn length . nub $ map head compartments
    where
      compartments = filter (liftM2 (&&) (with notAsLong) (with isDisjoint)) . combinations 3 $ getCompartment packages

entanglement :: [Int] -> Integer
entanglement = product . map fromIntegral

getCompartment :: [Int] -> [[Int]]
getCompartment packages =
  [ a | a <- subsequences packages, sum a == sum packages `div` 3]

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) (subsequences ns)

with :: ([Int] -> [Int] -> Bool) -> [[Int]] -> Bool
with f (x:y:xs) = f x y && with f (y:xs)
with _ _        = True

notAsLong :: [Int] -> [Int] -> Bool
notAsLong l1 l2 = length l1 <= length l2

isDisjoint :: [Int] -> [Int] -> Bool
isDisjoint l1 l2 = all (not . flip elem l2) l1

parseInput :: String -> [Int]
parseInput input =
  read <$> lines input
