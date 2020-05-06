module Days.Day21 (day21) where

import           Data.List
import           Data.List.Utils
import           Data.Ord
import           Days.ReadPuzzle

day21 :: IO ()
day21 = do
  input <- readPuzzle 21
  putStr "First: "
  first input
  putStr "Second: "
  second input

data ItemType = Weapon | Armor | Ring
  deriving (Show, Eq)

data Item = Item
  { _type   :: ItemType
  , _name   :: String
  , _cost   :: Int
  , _damage :: Int
  , _armor  :: Int
  } deriving (Show, Eq)

data Person = Person
  { _hitpoints :: Int
  , _attack    :: Int
  , _defense   :: Int
  , _money     :: Int
  } deriving (Show, Eq)

rawShop :: String
rawShop = "Weapons:    Cost  Damage  Armor\n\
\Dagger        8     4       0\n\
\Shortsword   10     5       0\n\
\Warhammer    25     6       0\n\
\Longsword    40     7       0\n\
\Greataxe     74     8       0\n\
\\n\
\Armor:      Cost  Damage  Armor\n\
\Leather      13     0       1\n\
\Chainmail    31     0       2\n\
\Splintmail   53     0       3\n\
\Bandedmail   75     0       4\n\
\Platemail   102     0       5\n\
\\n\
\Rings:      Cost  Damage  Armor\n\
\Damage+1    25     1       0\n\
\Damage+2    50     2       0\n\
\Damage+3   100     3       0\n\
\Defense+1   20     0       1\n\
\Defense+2   40     0       2\n\
\Defense+3   80     0       3"

shop :: [[Item]]
shop = map parseType . zip [Weapon, Armor, Ring] $ types
  where types = map tail $ split [""] $ lines rawShop
        parseType (t, l) = map (toItem t . words) l
        toItem t [n, c, d, a] = Item t n (read c) (read d) (read a)
        toItem _ _            = error "bad item"

parsePerson :: [Int] -> Person
parsePerson [hp, atk, def, money] = Person hp atk def money
parsePerson _                     = error "invalid person"

inventories :: [[Item]]
inventories = concat $ map select numbers
  where select xs = [concat [w, a, r] | let inv = zipWith takeN xs shop, w <- head inv, a <- last $ init inv, r <- last inv]
        numbers = [[1, a, r] | a <- [0..1], r <- [0..2]]
        takeN n = filter ((==) n . length) . subsequences

canDefeat :: Person -> Person -> Bool
canDefeat hero boss = attackNTimes boss hero ((blows hero boss) - 1) > 0
  where blows p1 p2 | _attack p1 /= _defense p2 = 1 + _hitpoints p2 `div` (_attack p1 - _defense p2)
        blows _ _ = maxBound
        attackNTimes p1 p2 n = _hitpoints p2 - n * (_attack p1 - _defense p2)

second :: String -> IO ()
second input = do
  let boss = parsePerson . (++ [0]) . map (read . last . words) $ lines input
  print . _money . head . reverse . sortBy (comparing _money) $ filter (not . flip canDefeat boss) heroes
      where heroes = map (parsePerson . toAttributes) inventories
            toAttributes x = [100, sumOf _damage x , sumOf _armor x, sumOf _cost x]
            sumOf f = sum . map f

first :: String -> IO ()
first input = do
  let boss = parsePerson . (++ [0]) . map (read . last . words) $ lines input
  print . _money . head . sortBy (comparing _money) $ filter (flip canDefeat boss) heroes
      where heroes = map (parsePerson . toAttributes) inventories
            toAttributes x = [100, sumOf _damage x , sumOf _armor x, sumOf _cost x]
            sumOf f = sum . map f
