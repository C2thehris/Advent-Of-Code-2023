{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.List (find, sort)
import Data.Map (Map, delete, empty, insert, lookup, toList)
import Data.Maybe (fromMaybe)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Read, Show, Enum, Eq, Ord)

data HandStrength = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Read, Show, Enum, Eq, Ord)

data Hand = Hand {strength :: HandStrength, cards :: [Card], value :: Int}
  deriving (Read, Show)

instance Eq Hand where
  (==) :: Hand -> Hand -> Bool
  h1 == h2 = (strength h1, cards h1) == (strength h2, cards h2)

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1 h2 = compare (strength h1, cards h1) (strength h2, cards h2)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseCard :: Char -> Card
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = Ten
parseCard 'J' = Jack
parseCard 'Q' = Queen
parseCard 'K' = King
parseCard 'A' = Ace
parseCard _ = error "Unknown Card"

parseCard' :: Char -> Card
parseCard' 'J' = Joker
parseCard' x = parseCard x

countCards :: (Foldable t, Num a1, Ord a2) => t a2 -> Data.Map.Map a2 a1
countCards =
  foldl
    ( \counts card ->
        let count = fromMaybe 0 (Data.Map.lookup card counts)
         in insert card (count + 1) counts
    )
    empty

getHandStrength :: (Foldable t, Ord a) => t a -> HandStrength
getHandStrength cards
  | 5 `elem` cardFrequencies = FiveOfAKind
  | 4 `elem` cardFrequencies = FourOfAKind
  | 3 `elem` cardFrequencies && 2 `elem` cardFrequencies = FullHouse
  | 3 `elem` cardFrequencies = ThreeOfAKind
  | length (filter (== 2) cardFrequencies) == 2 = TwoPair
  | 2 `elem` cardFrequencies = OnePair
  | otherwise = HighCard
  where
    cardFrequencies = map snd $ toList $ countCards cards

getHandStrength' :: (Foldable t) => t Card -> HandStrength
getHandStrength' cards
  | null cardsFrequencies = FiveOfAKind
  | head cardsFrequencies >= 5 - jokerCount = FiveOfAKind
  | head cardsFrequencies >= 4 - jokerCount = FourOfAKind
  | top2count >= 5 - jokerCount = FullHouse
  | head cardsFrequencies >= 3 - jokerCount = ThreeOfAKind
  | top2count >= 4 - jokerCount = TwoPair
  | head cardsFrequencies >= 2 - jokerCount = OnePair
  | otherwise = HighCard
  where
    origFrequencies = countCards cards
    jokerCount = fromMaybe 0 (Data.Map.lookup Joker origFrequencies)
    cardsFrequencies = reverse . sort $ map snd $ toList $ delete Joker origFrequencies
    top2count = sum $ take 2 cardsFrequencies

parseInput :: (Char -> Card) -> ([Card] -> HandStrength) -> [Char] -> Hand
parseInput cardParser handEvaluator raw =
  Hand {strength = handStrength, cards = hand, value = bid}
  where
    input = words raw
    hand = map cardParser (head input)
    handStrength = handEvaluator hand
    bid = read (input !! 1)

part1 :: IO String
part1 = show . sum . zipWith (*) [1 ..] . map value . sort . map (parseInput parseCard getHandStrength) <$> readLines "part1.txt"

-- part2 :: IO String
-- part2 = show . numWays' . parseInput' <$> readLines "part1.txt"

part2 :: IO String
part2 = show . sum . zipWith (*) [1 ..] . map value . sort . map (parseInput parseCard' getHandStrength') <$> readLines "part1.txt"

main :: IO ()
main = do
  part1Result <- part1
  putStrLn ("Part 1: " ++ part1Result ++ "\n")
  part2Result <- part2
  putStrLn ("Part 2: " ++ part2Result ++ "\n")
