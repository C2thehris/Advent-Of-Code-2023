module Main where

import Data.Char (isDigit)
import Data.List (tails)

parseAsIntAndRest :: (Read a) => [Char] -> [(a, String)]
parseAsIntAndRest str = case span isDigit str of
  (num, rest) -> [(read num, rest)]

getPartNumberLocations :: Int -> String -> [(Int, Int, Int)]
getPartNumberLocations pos line
  | null line = []
  | isDigit (head line) =
      let (value, restString) = head (parseAsIntAndRest line :: [(Int, String)])
          len = length (show value)
       in ((value, pos, pos + len - 1) : getPartNumberLocations (pos + len) restString)
  | otherwise = getPartNumberLocations (pos + 1) (tail line)

isSymbolNotPeriod :: Char -> Bool
isSymbolNotPeriod x = not (isDigit x) && (x /= '.')

checkAdjacentSymbol' :: [Char] -> (Int, Int) -> Bool
checkAdjacentSymbol' row (begin, end) =
  any isSymbolNotPeriod adjacent
  where
    adjacent = take (end - begin + 3) (drop (begin - 2) row)

checkAdjacentSymbol :: [Char] -> [Char] -> [Char] -> (Int, Int) -> Bool
checkAdjacentSymbol prevRow currentRow nextRow range =
  checkAdjacentSymbol' prevRow range || checkAdjacentSymbol' currentRow range || checkAdjacentSymbol' nextRow range

getPartTotal' :: [String] -> [[(Int, Int, Int)]] -> String -> Int
getPartTotal' [] _ _ = 0
getPartTotal' _ [] _ = 0
getPartTotal' [currentRow] partNumberLocations prevRow =
  foldl (\tot (value, begin, end) -> tot + if checkAdjacentSymbol currentRow prevRow [] (begin, end) then value else 0) 0 (head partNumberLocations)
getPartTotal' (currentRow : nextRow : restRow) (partNumberLocations : restPartNumberLocations) prevRow =
  foldl (\tot (value, begin, end) -> tot + if checkAdjacentSymbol prevRow currentRow nextRow (begin, end) then value else 0) 0 partNumberLocations + getPartTotal' (nextRow : restRow) restPartNumberLocations currentRow

getPartTotal :: [String] -> Int
getPartTotal input = getPartTotal' input (map (getPartNumberLocations 1) input) []

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

part1 :: IO String
part1 = show . getPartTotal <$> readLines "part1.txt"

groupWithWindow3 :: [a] -> [[a]]
groupWithWindow3 x = take 2 x : (filter ((>= 2) . length) . map (take 3) . tails) x

getNeighborsInGroup :: Int -> [(Int, Int, Int)] -> [Int]
getNeighborsInGroup _ [] = []
getNeighborsInGroup pos ((value, begin, end) : rest)
  | pos > end + 1 = getNeighborsInGroup pos rest
  | pos < begin - 1 = []
  | otherwise = value : getNeighborsInGroup pos rest

getNeighborPartCount :: Int -> [[(Int, Int, Int)]] -> Int
getNeighborPartCount pos = foldl (\tot g -> tot + length (getNeighborsInGroup pos g)) 0

getGearValue :: Int -> [[(Int, Int, Int)]] -> Int
getGearValue pos = foldl (\tot g -> tot * product (getNeighborsInGroup pos g)) 1

getGearTotal' :: [String] -> [[[(Int, Int, Int)]]] -> Int
getGearTotal' [] _ = 0
getGearTotal' _ [] = 0
getGearTotal' (row : restRows) (group : restGroups)
  | null restRows = total
  | otherwise = total + getGearTotal' restRows restGroups
  where
    total =
      foldl
        ( \tot (index, c) -> case c of
            '*' -> tot + if getNeighborPartCount index group == 2 then getGearValue index group else 0
            _ -> tot
        )
        0
        (zip [1 ..] row)

getGearTotal :: [String] -> Int
getGearTotal input = getGearTotal' input (groupWithWindow3 $ map (getPartNumberLocations 1) input)

part2 :: IO String
part2 = show . getGearTotal <$> readLines "part1.txt"

main :: IO ()
main = do
  part1Result <- part1
  putStrLn ("Part 1: " ++ part1Result ++ "\n")
  part2Result <- part2
  putStrLn ("Part 2: " ++ part2Result ++ "\n")
