import Data.Bifunctor (Bifunctor, bimap)

fullyContains :: Ord a => (a, a) -> (a, a) -> Bool
fullyContains (a, b) (c, d) =
  (a <= c && b >= d) || (c <= a && d >= b)

overlaps :: Ord a => (a, a) -> (a, a) -> Bool
overlaps (a, b) (c, d)
   = (c <= a && a <= d)
  || (c <= b && b <= d)
  || (a <= c && c <= b)
  || (a <= d && d <= b)

bimap1 :: Bifunctor p => (a -> b) -> p a a -> p b b
bimap1 f = bimap f f

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' f = bimap id tail . break f

type SectionPair = ((Int, Int), (Int, Int))

parseLine :: String -> SectionPair
parseLine = bimap1 section . break' (==',')
  where
    section = bimap1 read . break' (=='-')

solve :: ((Int, Int) -> (Int, Int) -> Bool) -> [SectionPair] -> Int
solve f = sum . map (fromEnum . uncurry f)

part1 :: [SectionPair] -> Int
part1 = solve fullyContains

part2 :: [SectionPair] -> Int
part2 = solve overlaps

main :: IO ()
main = do
  input <- map parseLine . lines <$> readFile "input/day4"
  let answer1 = part1 input
  print answer1
  let answer2 = part2 input
  print answer2
