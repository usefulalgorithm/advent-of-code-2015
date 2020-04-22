module Days.Day11 (day11) where

import           Data.Char
import           Data.List
import           Days.ReadPuzzle

day11 :: IO ()
day11 = do
  input <- init <$> readPuzzle 11
  putStr "First: "
  let res = go input
  print res
  putStr "Second: "
  print . go . step $ res

go :: String -> String
go input = head $ filter isValid $ iterate step input
    where isValid s = and $ ($ s) <$> [cond1, cond2, cond3]
          cond1 = not . null . (filter isAscending) . groupsOfThree
          groupsOfThree [_, _] = []
          groupsOfThree l      = (take 3 l) : groupsOfThree (tail l)
          isAscending (a:xs@(b:_)) = ((ord a + 1)== ord b) && isAscending xs
          isAscending _            = True
          cond2 = and . mapM all [(/= 'i'), (/= 'o'), (/= 'l')]
          cond3 = (<) 1 . length . (filter ((<) 1 . length)) . group

step :: String -> String
step = reverse . step' . reverse
  where step' ('z':xs) =  'a':(step' xs)
        step' (c:xs)   = (chr (1 + ord c)):xs
        step' _        = error "bad input"
