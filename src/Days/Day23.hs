module Days.Day23 (day23) where

import           Data.List
import           Days.ReadPuzzle

data Register = A | B

data State = State { a, b, n :: Int }
  deriving (Show, Eq, Ord)

type Program = [State -> State]

getRegister :: Register -> State -> Int
getRegister A s = a s
getRegister B s = b s

updateRegister :: (Int -> Int) -> Register -> State -> State
updateRegister fn A s = s { a = fn $ a s, n = 1 + n s }
updateRegister fn B s = s { b = fn $ b s, n = 1 + n s }

updateControl :: (Int -> Int) -> State -> State
updateControl fn s = s { n = fn $ n s }

updateCondition :: (Int -> Bool) -> Register -> (Int -> Int) -> State -> State
updateCondition cond r fn s =
  case cond $ getRegister r s of
    True  -> updateControl fn s
    False -> updateControl (+1) s

parseRegister :: String -> Register
parseRegister s
  | s == "a" = A
  | s == "b" = B
  | otherwise = error "invalid register"

parseOffset :: String -> (Int -> Int)
parseOffset ('+':off) = (+ (read off))
parseOffset ('-':off) = (+ negate (read off))
parseOffset _         = error "invalid offset"

parseInstruction :: String -> State -> State
parseInstruction str s =
  case words str of
    ["hlf", r] -> updateRegister (`div` 2) (parseRegister r) s
    ["tpl", r] -> updateRegister (* 3) (parseRegister r) s
    ["inc", r] -> updateRegister (+ 1) (parseRegister r) s
    ["jmp", offset] -> updateControl (parseOffset offset) s
    ["jie", r:",", offset] -> updateCondition even (parseRegister [r]) (parseOffset offset) s
    ["jio", r:",", offset] -> updateCondition (== 1) (parseRegister [r]) (parseOffset offset) s
    _ -> error "invalid instruction"

parseProgram :: String -> Program
parseProgram s = map parseInstruction $ lines s

programEnded :: Program -> State -> Bool
programEnded prog state = n state >= length prog

runInstruction :: Program -> State -> State
runInstruction prog state
  | n state < 0 = error "program counter out of bounds"
  | programEnded prog state = state
  | otherwise = (prog !! n state) state

runProgram :: Program -> State -> State
runProgram prog start =
  case find (programEnded prog) $ iterate (runInstruction prog) start of
    Nothing    -> error "program ended early"
    Just state -> state

day23 :: IO ()
day23 = do
  input <- readPuzzle 23
  let prog = parseProgram input
  putStr "First: "
  print . b . runProgram prog $ State 0 0 0
  putStr "Second: "
  print . b . runProgram prog $ State 1 0 0
