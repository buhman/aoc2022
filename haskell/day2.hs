data Shape1 = A | B | C
data Shape2 = X | Y | Z

parseLine :: String -> (Shape1, Shape2)
parseLine (ca:' ':cb:[]) = (p1 ca, p2 cb)
  where
    p1 'A' = A
    p1 'B' = B
    p1 'C' = C
    p2 'X' = X
    p2 'Y' = Y
    p2 'Z' = Z

parseInput :: String -> [(Shape1, Shape2)]
parseInput = map parseLine . lines

scoreShape :: Shape2 -> Int
scoreShape X = 1
scoreShape Y = 2
scoreShape Z = 3

scoreOutcome :: Shape2 -> Int
scoreOutcome X = 0
scoreOutcome Y = 3
scoreOutcome Z = 6

outcome1 :: Shape1 -> Shape2 -> Int
outcome1 i j
  = (scoreOutcome $ findOutcome i j)
  + (scoreShape j)
  where
    findOutcome A X = Y
    findOutcome A Y = Z
    findOutcome A Z = X
    findOutcome B X = X
    findOutcome B Y = Y
    findOutcome B Z = Z
    findOutcome C X = Z
    findOutcome C Y = X
    findOutcome C Z = Y

outcome2 :: Shape1 -> Shape2 -> Int
outcome2 i j
  = (scoreOutcome j)
  + (scoreShape $ findShape i j)
  where
    findShape A X = Z
    findShape A Y = X
    findShape A Z = Y
    findShape B X = X
    findShape B Y = Y
    findShape B Z = Z
    findShape C X = Y
    findShape C Y = Z
    findShape C Z = X

solve :: (Shape1 -> Shape2 -> Int) -> [(Shape1, Shape2)] -> Int
solve f = sum . map (uncurry f)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input/day2"
  let answer1 = solve outcome1 input
  print answer1
  let answer2 = solve outcome2 input
  print answer2
