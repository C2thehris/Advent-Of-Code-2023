module Main where

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseInput :: [String] -> [[Int]]
parseInput = map (map read . words)

differences :: (Num c) => [c] -> [c]
differences values = zipWith (-) (drop 1 values) (take (length values) values)

predictValues :: (Eq c, Num c) => [c] -> c
predictValues values
  | all (== 0) values = 0
  | otherwise = last values + predictValues (differences values)

part1 :: IO String
part1 = show . sum . map predictValues . parseInput <$> readLines "part1.txt"

part2 :: IO String
part2 = show . sum . map (predictValues . reverse) . parseInput <$> readLines "part1.txt"

main :: IO ()
main = do
  part1Result <- part1
  putStrLn ("Part 1: " ++ part1Result ++ "\n")
  part2Result <- part2
  putStrLn ("Part 2: " ++ part2Result ++ "\n")
