{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (lines, readFile, putStrLn, show)

import Text.Show (show)
import Data.Text.IO (readFile, putStrLn)
import Data.Text (Text, lines)
import Data.Text.Read (decimal)
import Data.List (sort)

data Input = Calories Int | NextElf

parseLine :: Text -> Either String Input
parseLine "" = pure NextElf
parseLine t = Calories . fst <$> decimal t

parseInput :: Text -> Either String [Input]
parseInput = sequence . (map parseLine) . lines

sums :: [Int] -> Input -> [Int]
sums xs NextElf = 0:xs
sums (x:xs) (Calories xi) = (xi + x):xs

maxSum :: Int -> [Int] -> Int
maxSum n = sum . take n . reverse . sort

solve :: Int -> [Input] -> Int
solve n = maxSum n . (foldl sums [0])

main :: IO ()
main = do
  input <- parseInput <$> readFile "input/day1"
  let (Right n) = solve 1 <$> input
  print n
  let (Right n) = solve 3 <$> input
  print n
