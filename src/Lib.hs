module Lib (run) where

import           Control.Monad
import           Language.Haskell.Interpreter

doDay :: Int -> IO ()
doDay n = do
  putStrLn $ "Day" ++ show n ++ ":"
  result <- runInterpreter $ setImports ["Days.Day" ++ show n] >> runStmt ("day" ++ show n)
  either (\_ -> putStrLn "Not implemented yet") return result

run :: [Int] -> IO ()
run args = do
  forM_ args doDay
