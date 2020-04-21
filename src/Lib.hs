module Lib (run) where

import           Control.Monad
import           Data.Either
import           Days.Day1
import           Days.Day2
import           Days.Day3
import           Days.Day4
import           Days.Day5
import           Days.Day6
import           Days.Day7
import           Language.Haskell.Interpreter

doDay :: Int -> IO ()
doDay n = do
  putStrLn $ "Day" ++ show n ++ ":"
  result <- runInterpreter $ setImports ["Days.Day" ++ show n] >> runStmt ("day" ++ show n)
  either (\_ -> putStrLn "Not implemented yet") return result

run :: [Int] -> IO ()
run args = do
  forM_ args doDay
