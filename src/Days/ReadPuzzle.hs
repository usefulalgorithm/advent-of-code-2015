{-# LANGUAGE CPP #-}
module Days.ReadPuzzle (readPuzzle) where

import           System.FilePath

readPuzzle :: Int -> IO String
readPuzzle n = readFile $ (takeDirectory __FILE__) ++ "/day" ++ (show n) ++ ".input.txt"
