import Data.Hash.MD5
import Data.String.Utils
import Data.Char
import Data.List

main :: IO ()
main = do
  putStr "First: "
  go 5
  putStr "Second: "
  go 6

go :: Int -> IO ()
go n = do
  key <- fmap (dropWhileEnd isSpace) $ readFile "input.txt"
  print $ head $ dropWhile (not . startswith (replicate n '0') . md5s . Str) $ map ((key++) . show) [0..]
