module Main where

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.ST as AST
import qualified Data.Array.Unboxed as AU
import Data.Either
import Data.List
import Text.Parsec

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

toF :: Instruction -> Int -> Int
toF ins =
  case ins of
    TurnOff _ -> max 0 . subtract 1
    TurnOn _ -> (+ 1)
    Toggle _ -> (+ 2)

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

applyInstructions :: [Instruction] -> AU.UArray LOC Int
applyInstructions ins =
  AST.runSTUArray $ do
    arr <- AST.newArray ((0, 0), (999, 999)) 0
    forM_ ins (applyInstruction arr)
    return arr

applyInstruction :: AST.STUArray s LOC Int -> Instruction -> ST s ()
applyInstruction arr i =
  forM_ (toLOCs . _range $ i) $ \loc -> do
    e <- AST.readArray arr loc
    AST.writeArray arr loc (f e)
  where
    f = toF i

run :: String -> IO ()
run s = do
  let ins = fromRight [] $ parse instructions "" s
  print . sum . AU.elems . applyInstructions $ ins

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStr "run: "
  run content
