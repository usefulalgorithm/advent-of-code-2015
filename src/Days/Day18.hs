module Days.Day18 where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.List
import           Days.ReadPuzzle

day18 :: IO ()
day18 = do
  input <- readPuzzle 18
  putStr "First: "
  first input
  putStr "Second: "
  second input

data Light = On | Off deriving (Eq)

rounds :: Int
rounds = 100

xSize, ySize :: Int
xSize = 100
ySize = 100

first :: String -> IO ()
first = go False

second :: String -> IO ()
second = go True

go :: Bool -> String -> IO ()
go stuck input = do
  print $ length $ filter (== On) $ elems $ runSTArray $ makeGrid stuck $ map toLight $ concat $ transpose $ lines input
    where toLight '#' = On
          toLight '.' = Off
          toLight _   = error "bad light"

makeGrid :: Bool -> [Light] -> ST s (STArray s (Int, Int) Light)
makeGrid stuck stuff = do
  grid <- newListArray ((1,1), (ySize, xSize)) stuff
  let doStuck = when (stuck == True) $ forM_ [(1,1), (1, xSize), (ySize, 1), (ySize, xSize)] $ flip (writeArray grid) On
  doStuck
  forM_ [1..rounds] $ \_ -> do
    gridList <- getAssocs grid
    oldGrid <- freeze grid
    forM_ gridList $ \(p, l) -> do
      let others = length $ filter (== On) $ map ((!) oldGrid) $ getNeighbors p
      writeArray grid p $ case (l, others) of
                            (On, 2) -> On
                            (_, 3)  -> On
                            _       -> Off
    doStuck
  return grid

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (y, x) = [(y', x') | y' <- [y-1..y+1], x' <- [x-1..x+1], x' > 0, y' > 0, x' <= xSize, y' <= ySize, (y', x') /= (y, x)]

testString :: String
testString = ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."
