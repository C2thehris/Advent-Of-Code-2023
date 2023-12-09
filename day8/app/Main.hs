import Data.Foldable (find)
import Data.Maybe (fromMaybe)

data Directions = Directions {current :: String, left :: String, right :: String}
  deriving (Read, Show)

parseDirections :: [Char] -> Directions
parseDirections [a, b, c, ' ', '=', ' ', '(', d, e, f, ',', ' ', g, h, i, ')'] = Directions {current = [a, b, c], left = [d, e, f], right = [g, h, i]}
parseDirections _ = error "Unable to parse Directions"

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseInput :: [String] -> (String, [Directions])
parseInput (first : _ : rest) = (first, map parseDirections rest)
parseInput _ = error "Bad Input"

getNewLocation :: Char -> [Directions] -> String -> String
getNewLocation 'L' directions location = left $ fromMaybe Directions {} (find ((== location) . current) directions)
getNewLocation 'R' directions location = right $ fromMaybe Directions {} (find ((== location) . current) directions)
getNewLocation _ _ _ = error "getNewLocation failed"

startingLocations :: [Directions] -> [String]
startingLocations = map current . filter ((== "A") . drop 2 . current)

runSim2' :: String -> (String, [Directions]) -> String -> Int
runSim2' path (originalPath, directions) location
  | ((== "Z") . drop 2) location = 0
  | null path = runSim2' originalPath (originalPath, directions) location
  | otherwise = runSim2' (tail path) (originalPath, directions) newLocation + 1
  where
    pathToTake = head path
    newLocation = getNewLocation pathToTake directions location

runSim2 :: (String, [Directions]) -> Int
runSim2 (path, directions) = foldl lcm 1 (map (runSim2' path (path, directions)) (startingLocations directions))

runSim' :: String -> String -> (String, [Directions]) -> Int
runSim' "ZZZ" _ _ = 0
runSim' location [] (originalPath, directions) = runSim' location originalPath (originalPath, directions)
runSim' location path (originalPath, directions) =
  runSim' newLocation (tail path) (originalPath, directions) + 1
  where
    pathToTake = head path
    newLocation = getNewLocation pathToTake directions location

runSim :: (String, [Directions]) -> Int
runSim (path, directions) = runSim' "AAA" path (path, directions)

part1 :: IO String
part1 = show . runSim . parseInput <$> readLines "part1.txt"

part2 :: IO String
part2 = show . runSim2 . parseInput <$> readLines "part1.txt"

main :: IO ()
main = do
  part1Result <- part1
  putStrLn ("Part 1: " ++ part1Result ++ "\n")
  part2Result <- part2
  putStrLn ("Part 2: " ++ part2Result ++ "\n")
