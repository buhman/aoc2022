import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Debug.Trace

--
-- begin parser
--

data BoxE = Empty | Box Char deriving Show

data Move = Move
  { qty :: Int
  , from :: Int
  , to :: Int
  }
  deriving Show

box :: ReadP BoxE
box = between (char '[') (char ']') (Box <$> get)

empty :: ReadP BoxE
empty = count 3 (char ' ') *> pure Empty

boxOrEmpty :: ReadP BoxE
boxOrEmpty = box +++ empty

row :: ReadP [BoxE]
row = sepBy1 boxOrEmpty (char ' ')

label :: ReadP Int
label = between (char ' ') (char ' ') (read . (:[]) <$> get)

columnLabels :: ReadP [Int]
columnLabels = sepBy1 (label) (char ' ')

stack :: ReadP [[BoxE]]
stack = sepBy1 row (char '\n')

move :: ReadP Move
move = do
  string "move "
  a <- read <$> munch1 isDigit
  string " from "
  b <- read <$> munch1 isDigit
  string " to "
  c <- read <$> munch1 isDigit
  pure (Move a b c)

moves :: ReadP [Move]
moves = sepBy1 move (char '\n')

stackMove :: ReadP ([[BoxE]], [Int], [Move])
stackMove = do
  s <- stack
  char '\n'
  l <- columnLabels
  char '\n'
  char '\n'
  m <- moves
  optional (char '\n')
  pure (s, l, m)

parse :: String -> ([[BoxE]], [Int], [Move])
parse s = last [ x | (x,"") <- readP_to_S stackMove s ]

--
-- begin solution
--

type Stack = IntMap [Char]

buildStack :: [[BoxE]] -> [Int] -> IntMap [Char]
buildStack [] l = M.fromList (zip l (repeat []))
buildStack (h:t) l = foldl build (buildStack t l) (zip l h)
  where
    build :: Stack -> (Int, BoxE) -> Stack
    build s (n, be) = M.update (Just . case be of
                                   Empty -> id
                                   Box c -> (c:)
                               ) n s

doMove1 :: Stack -> Move -> Stack
doMove1 s (Move 0 f t) = s
doMove1 s0 (Move n f t) = doMove1 s2 (Move (n - 1) f t)
  where
    (Just (c:_)) = M.lookup f s0
    s1 = M.update (Just . tail) f s0
    s2 = M.update (Just . (c:)) t s1

doMove2 :: Stack -> Move -> Stack
doMove2 s0 (Move n f t) = s2
  where
    (Just s) = M.lookup f s0
    s1 = M.update (Just . (drop n)) f s0
    s2 = M.update (Just . ((take n s)++)) t s1

topOfStack :: Stack -> [Int] -> String
topOfStack s = reverse . foldl (\a i -> ((top i):a)) []
  where
    top :: Int -> Char
    top = (\(Just (c:_)) -> c) . (flip M.lookup) s

main :: IO ()
main = do
  (boxe, labels, moves) <- parse <$> readFile "input/day5"
  let stack = buildStack boxe labels
  let stack1 = foldl doMove1 stack moves
  putStrLn $ topOfStack stack1 labels
  let stack2 = foldl doMove2 stack moves
  putStrLn $ topOfStack stack2 labels
