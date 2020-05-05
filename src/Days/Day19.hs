module Days.Day19 (day19) where

import           Data.Char
import           Data.List
import qualified Data.Map        as M
import           Days.ReadPuzzle

molecule :: String
molecule = "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr"

day19 :: IO ()
day19 = do
  input <- readPuzzle 19
  putStr "First: "
  first input
  putStr "Second: "
  second input

first :: String -> IO ()
first input = do
  print . subtract 1 . length . nub . concatMap applyFormula $ breakup particles
    where parseFormula [x, "=>", y] = (x, y)
          parseFormula _            = error "bad formula"
          toFormulas []         = M.empty
          toFormulas ((x,y):xs) = M.insertWith (++) x [y] $ toFormulas xs
          formulas = toFormulas . map (parseFormula . words) $ lines input
          particles = splitParticles molecule
          applyFormula (hs, m, ts) = map (\x -> hs ++ x ++ ts) $ findParticle m
          findParticle x = if M.member x formulas then formulas M.! x else [x]

breakup :: [String] -> [(String, String, String)]
breakup xs = zip3 hs m ts
  where hs = map concat $ init $ inits xs
        m = map head $ init $ tails xs
        ts = map (concat . tail) $ init $ tails xs

splitParticles :: String -> [String]
splitParticles (c1:c2:xs) | isUpper c1 && isLower c2 = [c1, c2]:splitParticles xs
splitParticles (c:xs) | isUpper c = [c]:splitParticles xs
splitParticles _ = []

second :: String -> IO ()
second _ = do
  print n
    where n = elements - rns - ars - 2*ys - 1
          elements = length particles
          byName s = length $ filter (== s) particles
          rns = byName "Rn"
          ars = byName "Ar"
          ys = byName "Y"
          particles = splitParticles molecule

-- backtracking algorithm... took too long to run
--
-- secondTLE :: String -> IO ()
-- secondTLE input = do
--   print $ toElectron reactions molecule
--     where parseReaction [x, "=>", y] = (y, x)
--           parseReaction _            = error "bad formula"
--           reactions = map (parseReaction . words) $ lines input

-- toElectron :: [(String, String)] -> String -> Int
-- toElectron reactions = fromJust . go 0
--   where go _ []  = Nothing
--         go c "e" = Just c
--         go _ s | countElem 'e' s >= 1 = Nothing
--         go c s   = listToMaybe . mapMaybe (go (c+1)) $ nub $ concat [swapPieces s p | p <- reactions]

-- swapPieces :: String -> (String, String) -> [String]
-- -- swapPieces s (y,x) = map (concat . combine pieces) $ (tail $ makeBinCombs (subtract 1 . length $ pieces) y x)
-- swapPieces s (y,x) = map (concat . combine pieces) $ map makeSingleCombs $ zip [0..] $ replicate (len - 1) $ replicate (len - 2) y
--   where pieces = split y s
--         len = length pieces
--         makeSingleCombs (n, xs) = let (a, b) = splitAt n xs in a ++ [x] ++ b
--         -- makeBinCombs 0 _ _ = [[]]
--         -- makeBinCombs c a b = map (a:) (makeBinCombs (c-1) a b) ++ map (b:) (makeBinCombs (c-1) a b)

-- combine :: [a] -> [a] -> [a]
-- combine [] ys     = ys
-- combine (x:xs) ys = x:combine ys xs
