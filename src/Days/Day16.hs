{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Days.Day16 (day16) where

import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Data.Data
import           Data.Maybe
import           Days.ReadPuzzle
import           GHC.Generics

day16 :: IO ()
day16 = do
  input <- readPuzzle 16
  putStr "First: "
  first input
  putStr "Second: "
  second input

first :: String -> IO ()
first input = do
  print . fst . head . filter (filterSue (repeat (cmpSue (==))) . snd) $ parseSue . words <$> lines input

second :: String -> IO ()
second input = do
  print . fst . head . filter (filterSue (map (cmpSue $) cmpFilter) . snd) $ parseSue . words <$> lines input
    where cmpFilter = [(==), (<), (==), (>), (==), (==), (>), (<), (==), (==)]

parseSue :: [String] -> (Int, Sue)
parseSue ("Sue":n:xs) = (read $ init n, fromJust . decode . BLU.fromString . (\s -> "{" ++ s ++ "}") . concat $ format xs)
parseSue _ = error "bad pattern"

format :: [String] -> [String]
format (x:xs) | (init x) `elem` constrFields (toConstr tickerTape) = ("\"" ++ init x ++ "\":"):format xs
format (x:xs) = x:format xs
format [] = []

filterSue :: [(Maybe Int -> Maybe Int -> Bool)] -> Sue -> Bool
filterSue cmp sue = and $ zipWith ($) (zipWith ($) cmp (sueToList tickerTape)) (sueToList sue)

cmpSue :: (Int -> Int -> Bool) -> Maybe Int -> Maybe Int -> Bool
cmpSue _ Nothing _         = True
cmpSue _ _ Nothing         = True
cmpSue f (Just a) (Just b) = a `f` b

sueToList :: Sue -> [Maybe Int]
sueToList sue = [children, cats, samoyeds, pomeranians, akitas, vizslas, goldfish, trees, cars, perfumes] <*> pure sue

data Sue = Sue
  { children    :: Maybe Int
  , cats        :: Maybe Int
  , samoyeds    :: Maybe Int
  , pomeranians :: Maybe Int
  , akitas      :: Maybe Int
  , vizslas     :: Maybe Int
  , goldfish    :: Maybe Int
  , trees       :: Maybe Int
  , cars        :: Maybe Int
  , perfumes    :: Maybe Int
  } deriving (Show, Generic, Data, Typeable)

instance FromJSON Sue where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }
instance ToJSON Sue

tickerTape :: Sue
tickerTape = Sue
  { children = Just 3
  , cats = Just 7
  , samoyeds = Just 2
  , pomeranians = Just 3
  , akitas = Just 0
  , vizslas = Just 0
  , goldfish = Just 5
  , trees = Just 3
  , cars = Just 2
  , perfumes = Just 1
  }
