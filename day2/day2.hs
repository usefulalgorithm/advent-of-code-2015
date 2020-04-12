import Data.List.Split (splitOn)
import Data.List

main = do
  content <- readFile "input.txt"
  putStr "First: "
  first content
  putStr "Second: "
  second content

first :: String -> IO ()
first s = do
  let dimensions = map sort $ fmap (map (read::String->Int)) $ map (splitOn "x") $ lines s
  let combs = map (combinations 2) dimensions
  let sorted = map ((map (foldl (*) 1)) . sort) combs
  let result = map sum $ map (zipWith (*) [3, 2, 2]) sorted
  print $ sum result

combinations k ns = filter ((k==).length) (subsequences ns)

toSides :: String -> [Int]
toSides s = sort $ fmap read $ splitOn "x" s

toPerimeter :: [Int] -> Int
toPerimeter = sum . zipWith (*) [2,2,0]

toVolume :: [Int] -> Int
toVolume = foldl (*) 1

second :: String -> IO ()
second s = do
  let sides = fmap toSides (lines s)
  let perimeters = fmap toPerimeter sides
  let volumes = fmap toVolume sides
  print $ sum $ zipWith (+) perimeters volumes
