import Data.Char (ord)
import Data.Set as S hiding (take, drop, map)

parseInput :: String -> [String]
parseInput = lines

priority :: Char -> Int
priority c
  | c >= 'a' && c <= 'z' = (ord c) - (ord 'a') + 1
  | c >= 'A' && c <= 'Z' = (ord c) - (ord 'A') + 27

setPriority :: Set Char -> Int
setPriority = S.foldr ((+) . priority) 0

groupP1 :: String -> [String]
groupP1 sack = [take bagSize sack, drop bagSize sack]
  where
    bagSize = length sack `div` 2

groupP2 :: [a] -> [[a]]
groupP2 [] = []
groupP2 (a:b:c:rest) = [a,b,c] : groupP2 rest

solve :: [[String]] -> Int
solve = sum . map solve'
  where
    solve' = setPriority . foldl1 intersection . map S.fromList

part1 :: [String] -> Int
part1 = solve . (map groupP1)

part2 :: [String] -> Int
part2 = solve . groupP2

main :: IO ()
main = do
  input <- parseInput <$> readFile "input/day3"
  let answer1 = part1 input
  print answer1
  let answer2 = part2 input
  print answer2

