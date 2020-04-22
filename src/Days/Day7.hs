module Days.Day7 (day7) where

import           Data.Bits
import           Data.Char
import qualified Data.Map        as M
import           Days.ReadPuzzle

day7 :: IO ()
day7 = do
  input <- readPuzzle 7
  putStr "First: "
  first input
  putStr "Second: "
  second input

parseGates :: String -> Circuit
parseGates s = M.fromList $ map (parseGate . words) $ lines s

parseGate :: [String] -> (String, Gate)
parseGate [x, "->", r] = (r, GateU id (parseTerm x))
parseGate ["NOT", x, "->", r] = (r, GateU complement (parseTerm x))
parseGate [x, "LSHIFT", y, "->", r] = (r, GateB (fir shiftL) (parseTerm x) (parseTerm y))
parseGate [x, "RSHIFT", y, "->", r] = (r, GateB (fir shiftR) (parseTerm x) (parseTerm y))
parseGate [x, "AND", y, "->", r] = (r, GateB (.&.) (parseTerm x) (parseTerm y))
parseGate [x, "OR", y, "->", r] = (r, GateB (.|.) (parseTerm x) (parseTerm y))
parseGate _ = error "unknown gate"

parseTerm :: String -> Term
parseTerm t@(t1:_)
  | isDigit t1 = TermC (read t)
  | isAlpha t1 = TermV t
  | otherwise = error "unknown term"
parseTerm _ = error "empty term"

fir :: (Word -> Int -> Word) -> (Word -> Word -> Word)
fir f a b = f a $ fromIntegral b

type Circuit = M.Map String Gate

data Gate = GateC Word
          | GateU (Word-> Word) Term
          | GateB (Word-> Word-> Word) Term Term

data Term = TermC Word
          | TermV String

evalCircuit :: Circuit -> String -> (Circuit, Word)
evalCircuit circuit target =
  case M.lookup target circuit of
    Just (GateC n) -> (circuit, n)
    Just (GateU f x) ->
      let (circuit', r) = evalTerm circuit x in
          (circuit', f r)
    Just (GateB f x y) ->
      let (circuit', r1) = evalTerm circuit x in
      let (circuit'', r2) = evalTerm circuit' y in
          (circuit'', f r1 r2)
    Nothing -> error "invalid target"

evalTerm :: Circuit -> Term -> (Circuit, Word)
evalTerm circuit (TermC n) = (circuit, n)
evalTerm circuit (TermV a) =
  let (circuit', n) = evalCircuit circuit a in
      (M.insert a (GateC n) circuit', n)

first :: String -> IO ()
first input = do
  let circuit = parseGates input
  print $ snd $ evalCircuit circuit "a"

second :: String -> IO ()
second input = do
  let circuit = parseGates input
  let val = snd $ evalCircuit circuit "a"
  let circuit' = M.insert "b" (GateC val) circuit
  print $ snd $ evalCircuit circuit' "a"
