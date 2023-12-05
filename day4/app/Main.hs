module Main where

import Data.Char (isDigit)
import Data.Foldable (find)
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseNumbers :: [Char] -> [Int]
parseNumbers [] = []
parseNumbers ('|' : _) = []
parseNumbers (current : rest)
  | isDigit current =
      let (value, restString) = head (reads (current : rest) :: [(Int, String)])
       in (value : parseNumbers restString)
  | otherwise = parseNumbers rest

increaseCardValue :: (Num a1) => Maybe a1 -> Maybe a2 -> Maybe a1
increaseCardValue x Nothing = x
increaseCardValue Nothing (Just _) = Just 1
increaseCardValue (Just x) (Just _) = Just (x * 2)

scoreCard' :: (Foldable t1, Foldable t2, Eq a2) => t2 a2 -> t1 a2 -> Maybe Int
scoreCard' winningNumbers yourNumbers =
  result
  where
    result =
      foldl
        (\curr e -> increaseCardValue curr (find (== e) winningNumbers))
        (Nothing :: Maybe Int)
        yourNumbers

scoreCard :: [Char] -> Int
scoreCard card =
  fromMaybe
    0
    ( scoreCard'
        (parseNumbers (drop 8 card))
        (parseNumbers (drop (fromJust (elemIndex '|' card) + 1) card))
    )

log2 :: (Num a, Integral t) => t -> a
log2 1 = 0
log2 x = log2 (x `div` 2) + 1

getWinCount :: (Num b) => [Char] -> b
getWinCount card =
  maybe
    0
    ((+ 1) . log2)
    ( scoreCard'
        (parseNumbers (drop 8 card))
        (parseNumbers (drop (fromJust (elemIndex '|' card) + 1) card))
    )

scoreCard2 :: [Int] -> [String] -> Int
scoreCard2 [] _ = 0
scoreCard2 _ [] = 0
scoreCard2 (thisCount : restCounts) (thisCard : restCards) =
  thisCount + scoreCard2 newCounts restCards
  where
    score = getWinCount thisCard
    newCounts = map (+ thisCount) (take score restCounts) ++ drop score restCounts

part1 :: IO String
part1 = show . foldl (\curr e -> curr + scoreCard e) (0 :: Int) <$> readLines "part1.txt"

one :: Int
one = 1 :: Int

part2 :: IO String
part2 = show . scoreCard2 (map (const one) [one ..]) <$> readLines "part1.txt"

main :: IO ()
main = do
  part1Result <- part1
  putStrLn ("Part 1: " ++ part1Result ++ "\n")
  part2Result <- part2
  putStrLn ("Part 2: " ++ part2Result ++ "\n")