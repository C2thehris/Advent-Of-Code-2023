{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Char (isDigit)
import Data.List (intercalate, stripPrefix)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

readLines :: FilePath -> IO [String]
readLines filename = do
  fileHandle <- openFile filename ReadMode
  rawContents <- hGetContents fileHandle
  return (lines rawContents)

firstNumLastNum :: String -> String
firstNumLastNum line =
  let numeric = filter isDigit line
   in [head numeric, last numeric]

replaceWordWithDigit :: String -> Char
replaceWordWithDigit (stripPrefix "one" -> Just _) = '1'
replaceWordWithDigit (stripPrefix "two" -> Just _) = '2'
replaceWordWithDigit (stripPrefix "three" -> Just _) = '3'
replaceWordWithDigit (stripPrefix "four" -> Just _) = '4'
replaceWordWithDigit (stripPrefix "five" -> Just _) = '5'
replaceWordWithDigit (stripPrefix "six" -> Just _) = '6'
replaceWordWithDigit (stripPrefix "seven" -> Just _) = '7'
replaceWordWithDigit (stripPrefix "eight" -> Just _) = '8'
replaceWordWithDigit (stripPrefix "nine" -> Just _) = '9'
replaceWordWithDigit (x : _) = if isDigit x then x else '\0'
replaceWordWithDigit [] = '\0'

doTheThing :: String -> String
doTheThing [] = []
doTheThing x = replaceWordWithDigit x : doTheThing (tail x)

firstNumLastNumWords :: String -> String
firstNumLastNumWords line =
  let numeric = filter isDigit (doTheThing line)
   in [head numeric, last numeric]

part1 :: IO String
part1 = do
  part1Input <- readLines "part1.txt"
  return $ show $ foldl (\b a -> b + read (firstNumLastNum a)) (0 :: Integer) part1Input

part2 :: IO String
part2 = do
  part2Input <- readLines "part1.txt"
  return $ show $ foldl (\b a -> b + read (firstNumLastNumWords a)) (0 :: Integer) part2Input

debug :: IO ()
debug = do
  part2Input <- readLines "part1.txt"
  putStrLn (intercalate "\n" (map firstNumLastNumWords part2Input))

main :: IO ()
main = do
  part1Result <- part1
  putStrLn ("Part 1: " ++ part1Result ++ "\n")
  part2Result <- part2
  putStrLn ("Part 2: " ++ part2Result ++ "\n")
