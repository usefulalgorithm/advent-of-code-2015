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

toF :: Instruction -> Bool -> Bool
toF ins =
  case ins of
    TurnOff _ -> const False
    TurnOn _ -> const True
    Toggle _ -> not

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

applyInstructions :: [Instruction] -> AU.UArray LOC Bool
applyInstructions ins =
  AST.runSTUArray $ do
    arr <- AST.newArray ((0, 0), (999, 999)) False
    forM_ ins (applyInstruction arr)
    return arr

applyInstruction :: AST.STUArray s LOC Bool -> Instruction -> ST s ()
applyInstruction arr i =
  forM_ (toLOCs . _range $ i) $ \loc -> do
    e <- AST.readArray arr loc
    AST.writeArray arr loc (f e)
  where
    f = toF i

ins s = fromRight [] $ parse instructions "" s

first :: String -> IO ()
first s = do
  print . length . filter id . AU.elems . applyInstructions $ ins s

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStr "First: "
  first content
