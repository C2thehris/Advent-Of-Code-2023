{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Foldable (foldl')
import Data.List (intercalate, stripPrefix)
import GHC.Unicode (isDigit)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

allValid :: String -> Bool
allValid [] = True
allValid game
  | isDigit (head game) =
      let (count, post) : _ = reads game :: [(Integer, String)]
       in case getColor post of
            "red" -> (count <= 12) && allValid post
            "green" -> (count <= 13) && allValid post
            _ -> (count <= 14) && allValid post
  | otherwise = allValid (tail game)

gameValue1 :: String -> Integer
gameValue1 (stripPrefix "Game " -> Just gameValueGame)
  | allValid game = value
  | otherwise = 0
  where
    (value, game) = head (reads gameValueGame :: [(Integer, String)])
gameValue1 _ = 0

maxColors :: String -> Integer -> Integer -> Integer -> (Integer, Integer, Integer)
maxColors [] r g b = (r, g, b)
maxColors game r g b
  | isDigit (head game) =
      let (count, post) : _ = reads game :: [(Integer, String)]
       in case getColor post of
            "red" -> maxColors post (max r count) g b
            "green" -> maxColors post r (max g count) b
            _ -> maxColors post r g (max b count)
  | otherwise = maxColors (tail game) r g b

gameValue2 :: String -> Integer
gameValue2 (stripPrefix "Game " -> Just gameValueGame) = r * g * b
  where
    (_, game) = head (reads gameValueGame :: [(Integer, String)])
    (r, g, b) = maxColors game 0 0 0
gameValue2 _ = 0

getColor :: String -> String
getColor (stripPrefix "red" -> Just _) = "red"
getColor (stripPrefix "blue" -> Just _) = "blue"
getColor (stripPrefix "green" -> Just _) = "green"
getColor (_ : rest) = getColor rest
getColor [] = []

part1 :: IO String
part1 = show . foldl' (\tot game -> tot + gameValue1 game) 0 <$> readLines "part1.txt"

part2 :: IO String
part2 = show . foldl' (\tot game -> tot + gameValue2 game) 0 <$> readLines "part1.txt"

debug :: IO ()
debug = do
  input <- readLines "part1.txt"
  putStrLn (intercalate "\n" (map (show . gameValue1) input))

main :: IO ()
main = do
  part1Result <- part1
  putStrLn ("Part 1: " ++ part1Result ++ "\n")
  part2Result <- part2
  putStrLn ("Part 2: " ++ part2Result ++ "\n")
