module Days.Day4 (day4) where
import           Data.Hash.MD5
import           Data.String.Utils
import           Days.ReadPuzzle

day4 :: IO ()
day4 = do
  input <- strip <$> readPuzzle 4
  putStr "First: "
  go input 5
  putStr "Second: "
  go input 6

go :: String -> Int -> IO ()
go key n = do
  print $ head $ dropWhile (not . startswith (replicate n '0') . md5s . Str) $ map ((key++) . (show :: Int -> String)) [0..]
