{-# LANGUAGE NumericUnderscores #-}

import Text.ParserCombinators.ReadP
import qualified Data.Map as M
import Data.Char (isDigit)
import Numeric (showInt)
import Data.List (sortBy)

data File = File
  { fileName :: String
  , fileSize :: Int
  }

data Dir = Dir
  { dirName :: String
  , dirSize :: Int
  , dirDirs :: M.Map String Dir
  , dirFiles :: M.Map String File
  }

data FsZipper = FsZipper
  { zipPath :: [Dir]
  , zipCur :: Dir
  }

data Direction = Up | Down String | Root deriving (Show)

data Output = OutputFile File | OutputDir Dir deriving (Show)

data Command = Cd Direction | Ls deriving (Show)

data TerminalLine = Command Command | Output Output deriving (Show)

spaces :: ReadP String
spaces = many1 (char ' ')

name :: ReadP String
name = munch1 (/= '\n')

number :: ReadP Int
number = read <$> munch1 isDigit

command :: ReadP Command
command = char '$' *> spaces *> (Cd <$> cd) +++ (Ls <$ ls)
  where
    ls = string "ls"
    cd = string "cd" *> spaces *> direction
    direction = (Root <$ char '/')
            <++ (Up <$ string "..")
            <++ (Down <$> name)

output :: ReadP Output
output = (OutputDir . dir <$> dirName) +++ (OutputFile <$> file)
  where
    dirName = string "dir" *> spaces *> name
    dir name = (Dir name 0 M.empty M.empty)
    file = flip File <$> number <*> (spaces *> name)


terminalLine :: ReadP TerminalLine
terminalLine = (Output <$> output) +++ (Command <$> command)

terminalOutput :: ReadP [TerminalLine]
terminalOutput = endBy1 terminalLine (char '\n')

parseInput :: String -> [TerminalLine]
parseInput s = last [ x | (x,"") <- readP_to_S terminalOutput s ]

-- show

instance Show File where
  showsPrec _ (File name size) = showString "- "
                               . showString name
                               . showString " (file, size="
                               . showInt size
                               . showChar ')'

instance Show Dir where
  showsPrec _ dir = showDir 0 dir
    where
      showDir :: Int -> Dir -> ShowS
      showDir indent (Dir name size dirs files)
        = showString (take (indent * 2) $ repeat ' ')
        . showString "- "
        . showString name
        . showString " (dir)"
        . showDirs (indent+1) (M.elems dirs)
        . showFiles (indent+1) (M.elems files)

      showDirs :: Int -> [Dir] -> ShowS
      showDirs indent ds = foldr ((.) . (\x -> showChar '\n' . showDir indent x)) id ds

      showFile :: Int -> File -> ShowS
      showFile indent file = showString (take (indent * 2) $ repeat ' ')
                           . shows file

      showFiles :: Int -> [File] -> ShowS
      showFiles indent = foldr ((.) . (\x -> showChar '\n' . showFile indent x)) id

instance Show FsZipper where
  showsPrec _ (FsZipper path cur) = showString "FsZipper {zipPath = ["
                                  . showDirs path
                                  . showString "], zipCur = "
                                  . showDir cur
                                  . showChar '}'
    where
      showDir :: Dir -> ShowS
      showDir d = showString $ dirName d

      showDirs :: [Dir] -> ShowS
      showDirs [] = id
      showDirs (d:ds) = showDir d
                      . foldr ((.) . (\x -> showChar ',' . showDir x)) id ds

-- zipper

moveDown :: String -> FsZipper -> FsZipper
moveDown s (FsZipper path cur) = FsZipper (cur:path) dir
  where
    (Just dir) = M.lookup s (dirDirs cur)

moveUp :: FsZipper -> FsZipper
moveUp (FsZipper (p:ps) cur) = FsZipper ps dir
  where
    dir = p { dirDirs = M.insert (dirName cur) cur (dirDirs p) }

moveRoot :: FsZipper -> FsZipper
moveRoot (FsZipper [] cur) = FsZipper [] cur
moveRoot z = moveRoot $ moveUp z

addFile :: File -> FsZipper -> FsZipper
addFile f (FsZipper path cur) = FsZipper path cur'
  where
    cur' = cur
      { dirFiles = M.insert (fileName f) f (dirFiles cur)
      }

addDir :: Dir -> FsZipper -> FsZipper
addDir d (FsZipper path cur) = FsZipper path cur'
  where
    cur' = cur
      { dirDirs = M.insert (dirName d) d (dirDirs cur)
      }

--

interpretTL :: FsZipper -> TerminalLine -> FsZipper
interpretTL z (Command Ls) = z
interpretTL z (Command (Cd Up)) = moveUp z
interpretTL z (Command (Cd (Down s))) = moveDown s z
interpretTL z (Command (Cd Root)) = moveRoot z
interpretTL z (Output (OutputFile f)) = addFile f z
interpretTL z (Output (OutputDir d)) = addDir d z

interpret :: [TerminalLine] -> Dir
interpret = zipCur . moveRoot . foldl interpretTL empty
  where
    empty = FsZipper [] (Dir "/" 0 M.empty M.empty)

--

calcDirSize :: Dir -> Dir
calcDirSize d@(Dir _ _ dirs files)
  = d { dirSize = filesSize files + dirsSize dirs }
  where
    filesSize = sum . map fileSize . M.elems
    dirsSize = sum . map (dirSize . calcDirSize) . M.elems

findDirs :: (Dir -> Bool) -> Dir -> [Dir]
findDirs p d@(Dir _ _ dirs _) =
  if p d' then d':rest else rest
  where
    rest = concatMap (findDirs p) dirs
    d' = calcDirSize d

part1 :: Dir -> Int
part1 = sum . map dirSize . findDirs pred
  where
    pred = (<=100000) . dirSize

part2 :: Dir -> Int
part2 root = dirSize . head . sortBy sortDir . findDirs pred $ root
  where
    fsTotal = 70_000_000

    fsUsed = dirSize $ calcDirSize root

    fsFree = fsTotal - fsUsed

    fsWant = 30_000_000

    pred = (>=fsWant) . (fsFree+) . dirSize

    sortDir a b = compare (dirSize a) (dirSize b)

--

main :: IO ()
main = do
  rootDir <- interpret . parseInput <$> readFile "../input/day7"
  print $ part1 rootDir
  print $ part2 rootDir
