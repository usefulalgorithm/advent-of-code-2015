module Days.Day6 (day6) where

import           Days.ReadPuzzle

import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array.ST      as AST
import qualified Data.Array.Unboxed as AU
import           Data.Either
import           Text.Parsec

type LOC = (Int, Int)

data Range =
  Range LOC
        LOC
  deriving (Show)

data Instruction
  = TurnOff { _range :: Range }
  | TurnOn { _range :: Range }
  | Toggle { _range :: Range }
  deriving (Show)

toEngF :: Instruction -> Bool -> Bool
toEngF ins =
  case ins of
    TurnOff _ -> const False
    TurnOn _  -> const True
    Toggle _  -> not

toElvF :: Instruction -> Int -> Int
toElvF ins =
  case ins of
    TurnOff _ -> max 0 . subtract 1
    TurnOn _  -> (+ 1)
    Toggle _  -> (+ 2)

toLOCs :: Range -> [LOC]
toLOCs (Range (x0, y0) (x1, y1)) = [(x, y) | x <- [x0 .. x1], y <- [y0 .. y1]]

int :: Parsec String u Int
int = read <$> many1 digit

pair :: Parsec String u LOC
pair = do
  x <- int
  _ <- char ','
  y <- int
  return (x, y)

range :: Parsec String u Range
range = do
  p0 <- pair
  _ <- spaces *> string "through" <* spaces
  p1 <- pair
  return $ Range p0 p1

toInstruction :: Parsec String u (Range -> Instruction)
toInstruction =
  (const Toggle <$> try (string "toggle")) <|>
  (const TurnOn <$> try (string "turn on")) <|>
  (const TurnOff <$> try (string "turn off"))

instruction :: Parsec String u Instruction
instruction = do
  f <- toInstruction
  _ <- spaces
  r <- range
  return $ f r

instructions :: Parsec String u [Instruction]
instructions = sepEndBy instruction spaces

applyEngInstructions :: [Instruction] -> AU.UArray LOC Bool
applyEngInstructions ins =
  AST.runSTUArray $ do
    arr <- AST.newArray ((0, 0), (999, 999)) False
    forM_ ins (applyEngInstruction arr)
    return arr

applyEngInstruction :: AST.STUArray s LOC Bool -> Instruction -> ST s ()
applyEngInstruction arr i =
  forM_ (toLOCs . _range $ i) $ \loc -> do
    e <- AST.readArray arr loc
    AST.writeArray arr loc (f e)
  where
    f = toEngF i

applyElvInstructions :: [Instruction] -> AU.UArray LOC Int
applyElvInstructions ins =
  AST.runSTUArray $ do
    arr <- AST.newArray ((0, 0), (999, 999)) 0
    forM_ ins (applyElvInstruction arr)
    return arr

applyElvInstruction :: AST.STUArray s LOC Int -> Instruction -> ST s ()
applyElvInstruction arr i =
  forM_ (toLOCs . _range $ i) $ \loc -> do
    e <- AST.readArray arr loc
    AST.writeArray arr loc (f e)
  where
    f = toElvF i

first :: String -> IO ()
first s = do
  let ins = fromRight [] $ parse instructions "" s
  print . length . filter id . AU.elems . applyEngInstructions $ ins

second :: String -> IO ()
second s = do
  let ins = fromRight [] $ parse instructions "" s
  print . sum . AU.elems . applyElvInstructions $ ins

day6 :: IO ()
day6 = do
  input <- readPuzzle 6
  putStr "First: "
  first input
  putStr "Second: "
  second input
