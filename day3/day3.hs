import qualified Data.Set as S

main = do
  content <- readFile "input.txt"
  putStr "First: "
  first content

first :: String -> IO ()
first s = do
  let start = (0,0)
  let set = S.singleton start
  print $ S.size $ foldl (\s p -> S.insert p s) set $ scanl nextPoint start s

nextPoint :: (Int, Int) -> Char -> (Int, Int)
nextPoint (x, y) c = case c of
                       '^' -> (x, y+1)
                       'v' -> (x, y-1)
                       '<' -> (x-1, y)
                       '>' -> (x+1, y)
                       _ -> (x, y)
