module Days.Day12 (day12) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU (fromString)
import qualified Data.HashMap.Strict       as M
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Days.ReadPuzzle

day12 :: IO ()
day12 = do
  input <- readPuzzle 12
  putStr "First: "
  go processJson input
  putStr "Second: "
  go processJson' input

go :: (Value -> Int) -> String -> IO ()
go process = do
  print . process . fromJust . decode . BLU.fromString

processJson :: Value -> Int
processJson (Number n)   = fromJust . toBoundedInteger $ n
processJson (Array a)    = sum $ processJson <$> V.toList a
processJson (Object obj) = sum $ processJson <$> M.elems obj
processJson _            = 0

processJson' :: Value -> Int
processJson' (Object obj) | String (T.pack "red") `elem` M.elems obj = 0
processJson' (Object obj) = sum $ processJson' <$> M.elems obj
processJson' (Number n)   = fromJust . toBoundedInteger $ n
processJson' (Array a)    = sum $ processJson' <$> V.toList a
processJson' _            = 0
