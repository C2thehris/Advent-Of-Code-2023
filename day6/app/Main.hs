module Main where

import qualified Data.Bifunctor
import GHC.Float (ceilingDouble, floorDouble)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

getDist :: (Num a) => a -> a -> a
getDist t x = (t - x) * x

extractNums :: [Char] -> [Int]
extractNums line =
  let parts = words line
   in (map read $ drop 1 parts) :: [Int]

parseInput :: [String] -> [(Int, Int)]
parseInput [first, second] =
  zip (extractNums first) (extractNums second)
parseInput _ = [(0, 0)]

numWays :: (Ord a, Num a, Enum a) => (a, a) -> Int
numWays (t, d) =
  length $ filter (> d) $ map (getDist t) x
  where
    x = [1 .. t]

part1 :: IO String
part1 = show . product . map numWays . parseInput <$> readLines "part1.txt"

parseInput' :: [String] -> (Int, Int)
parseInput' [first, second] =
  let pairs = parseInput [first, second]
      strPairs = map (Data.Bifunctor.bimap show show) pairs
      joinedStrs = foldl (\(currL, currR) (l, r) -> (currL ++ l, currR ++ r)) ("", "") strPairs
   in Data.Bifunctor.bimap read read joinedStrs
parseInput' _ = (0, 0)

numWays' :: (Int, Int) -> Int
numWays' (tOrig, dOrig) =
  let t = fromIntegral tOrig
      d = fromIntegral dOrig
      lower = ceilingDouble $ (t - sqrt (t * t - 4 * d)) / 2
      higher = floorDouble $ (t + sqrt (t * t - 4 * d)) / 2
   in (higher - lower + 1) :: Int

part2 :: IO String
part2 = show . numWays' . parseInput' <$> readLines "part1.txt"

main :: IO ()
main = do
  part1Result <- part1
  putStrLn ("Part 1: " ++ part1Result ++ "\n")
  part2Result <- part2
  putStrLn ("Part 2: " ++ part2Result ++ "\n")
