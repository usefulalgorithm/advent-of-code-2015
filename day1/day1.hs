main = do
  x <- readFile "input.txt"
  putStr "First: "
  print $ first x
  putStr "Second: "
  print $ second x
first x =
  foldl (\acc x -> if x == '(' then acc+1 else if x == ')' then acc-1 else acc) 0 x
second x =
  fst $ head $ filter (\(x, y) -> y < 0) $ zip [0..] $ scanl (\acc x -> if x == '(' then acc+1 else if x == ')' then acc-1 else acc) 0 x
