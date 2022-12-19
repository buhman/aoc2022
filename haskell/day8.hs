import Data.Char (ord)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (sort)

data Point = P { pX :: Int, pY :: Int } deriving (Show, Ord, Eq)

type Grid = M.Map Point Int

readChar :: Char -> Int
readChar c = ord c - ord '0'

parseLine :: Grid -> (Int, [Char]) -> Grid
parseLine g (y, cs) = foldl f g . zip [0..] . map readChar $ cs
  where
    f g (x, i) = M.insert (P x y) i g

parseInput :: String -> Grid
parseInput = foldl parseLine M.empty . zip [0..] . lines

data Bound = Bound
  { minX :: Int
  , maxX :: Int
  , minY :: Int
  , maxY :: Int
  } deriving Show

getBound :: Grid -> Bound
getBound m = Bound { minX = bound min pX m
                   , maxX = bound max pX m
                   , minY = bound min pY m
                   , maxY = bound max pY m
                   }
  where
    bound f g = foldl f 0 . map g . M.keys

line :: (Point -> Point) -> Grid -> Bound -> Point -> [Int]
line f g (Bound minX maxX minY maxY) = map (g M.!)
                                     . takeWhile inBounds
                                     . drop 1
                                     . iterate f
  where
    inBounds (P x y) = x >= minX && x <= maxX
                    && y >= minY && y <= maxY

cardinals :: [(Point -> Point)]
cardinals = [ \(P x y) -> P x     (y-1)
            , \(P x y) -> P (x-1) y
            , \(P x y) -> P x     (y+1)
            , \(P x y) -> P (x+1) y
            ]

treelines :: Grid -> Bound -> Point -> [[Int]]
treelines g b p = map (\f -> line f g b p) cardinals

treeVisible :: Int -> [[Int]] -> Bool
treeVisible i = any lineVisible
  where
    lineVisible = all (<i)

part1 :: Grid -> Int
part1 g = length . filter (==True) . map f $ M.assocs g
  where
    b = getBound g
    f (p, i) = treeVisible i $ treelines g b p

countVisible :: Int -> [[Int]] -> [Int]
countVisible i = map (length . visible)
  where
    visible [] = []
    visible (x:xs)
      |     x < i = x:(visible xs)
      | otherwise = x:[]

part2 :: Grid -> Int
part2 g = last . sort . map f $ M.assocs g
  where
    b = getBound g
    f (p, i) = foldl (*) 1 . countVisible i $ treelines g b p

main :: IO ()
main = do
  input <- parseInput <$> readFile "../input/day8"
  print $ part1 input
  print $ part2 input
