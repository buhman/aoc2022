import qualified Data.Set as S

startOfPacket :: Int -> [Char] -> Int
startOfPacket n s = go 0 s
  where
    go ix si =
      case n == S.size (S.fromList (take n si)) of
        True  -> n + ix
        False -> go (ix+1) $ drop 1 si

main :: IO ()
main = do
  input <- readFile "input/day6"
  let part1 = startOfPacket 4 input
  print part1
  let part2 = startOfPacket 14 input
  print part2
