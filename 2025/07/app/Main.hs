-- AoC 2025 Day 07
module Main (main) where

import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> do
      brd <- parseFile input
      let (part1Result, finalSplitState) = doPart1 brd
      putStrLn $ "Part 1: " ++ show part1Result
      putStrLn $ "Part 2: " ++ show (countRoutes brd)
      putStrLn "Final board flow state:"
      dump finalSplitState
    _ -> do
      name <- getProgName
      hPutStr stderr $ "usage: " ++ name ++ " INPUT\n"
      exitFailure

data AdvanceResult = Split {left :: Point, right :: Point} | Continue Point | Out

data Board = Board
  { start :: Point,
    grid :: [Row]
  }

data Direction = DUp | DDown | DLeft | DRight
  deriving (Enum, Eq, Show)

data HasSplit = DidSplit | NoSplit

type Point = (Int, Int)

type Routes = Map Point Int

type Row = [Char]

data SplitState = SplitState
  { sboard :: Board,
    beams :: Set Point,
    visited :: Set Point,
    splits :: Int
  }

dirVersor :: Direction -> Point
dirVersor DDown = (1, 0)
dirVersor DLeft = (0, -1)
dirVersor DRight = (0, 1)
dirVersor DUp = (-1, 0)

infixl 6 |+|

(|+|) :: Point -> Direction -> Point
(a, b) |+| dir = (a + dx, b + dy)
  where
    (dx, dy) = dirVersor dir

advance :: SplitState -> SplitState
advance SplitState {sboard = brd, beams = bs, visited = v, splits = nSplits} =
  SplitState
    { sboard = brd,
      beams = newBeams,
      visited = Set.union v newBeams,
      splits = nSplits + splitsThisTick
    }
  where
    (newBeams, splitsThisTick) = Set.foldl accumulate (Set.empty, 0) bs
    accumulate :: (Set Point, Int) -> Point -> (Set Point, Int)
    accumulate (accBeams, accSplits) p =
      case hasSplit of
        DidSplit -> (accBeams', accSplits + 1)
        NoSplit -> (accBeams', accSplits)
      where
        (newPts, hasSplit) = tickBeam p
        accBeams' = Set.union accBeams newPts

    tickBeam :: Point -> (Set Point, HasSplit)
    tickBeam p =
      case advanceIn brd p of
        Continue p' -> (Set.singleton p', NoSplit)
        Out -> (Set.empty, NoSplit)
        Split {left = l, right = r} -> (Set.fromList [l, r], DidSplit)

advanceIn :: Board -> Point -> AdvanceResult
advanceIn brd p =
  case cell of
    Just '.' -> Continue p'
    Just '^' -> Split {left = p' |+| DLeft, right = p' |+| DRight}
    Just ch -> error $ "invalid cell character: " ++ [ch]
    Nothing -> Out
  where
    p' = p |+| DDown
    cell = charAt p' brd

areLinesABoard :: [Row] -> Bool
areLinesABoard [] = True
areLinesABoard (x : xs) = all (\y -> length y == length x) xs

charAt :: Point -> Board -> Maybe Char
charAt (x, y) brd = row x brd >>= get y

countRoutes :: Board -> Int
countRoutes brd@Board {start = s} =
  let (_, count) = countRoutesAt Map.empty s in count
  where
    countRoutesAt :: Routes -> Point -> (Routes, Int)
    countRoutesAt rts p =
      case Map.lookup p rts of
        Just c -> (rts, c)
        Nothing -> countRoutesFrom rts p

    countRoutesFrom :: Routes -> Point -> (Routes, Int)
    countRoutesFrom rts p =
      case advanceIn brd p of -- we can assert that p is not a split point, because S is always a '.' and splits are never contiguous
        Continue p' -> let (rts', count) = countRoutesAt rts p' in (Map.insert p count rts', count) -- if we continue, the count of the current point is the same as the next
        Split {left = l, right = r} ->
          (Map.insert p totalCount rts', totalCount) -- if we split, the count is the sum of the two branches
          where
            (rts', totalCount) =
              foldl
                ( \(accRts, accCount) pt ->
                    let (newRts, cnt) = countRoutesAt accRts pt
                     in (newRts, accCount + cnt)
                )
                (rts, 0)
                (filterPointsIn brd [l, r])
        Out -> (Map.insert p 1 rts, 1) -- if we go out, there's one route (the current one)

dims :: Board -> (Int, Int)
dims Board {grid = []} = (0, 0)
dims Board {grid = g@(firstRow : _)} = (length g, length firstRow)

doPart1 :: Board -> (Int, SplitState)
doPart1 brd =
  nextRound $ initialSplitState brd
  where
    nextRound :: SplitState -> (Int, SplitState)
    nextRound st@SplitState {beams = bs, splits = nSplits}
      | Set.null bs = (nSplits, st)
      | otherwise = nextRound $ advance st

dump :: SplitState -> IO ()
dump SplitState {sboard = Board {grid = []}, beams = _, visited = _} = pure ()
dump SplitState {sboard = brd@Board {start = s, grid = g}, beams = _, visited = v} = do
  let (rows, cols) = dims brd
  mapM_ (putStrLn . renderRow cols) [0 .. rows - 1]
  where
    renderRow :: Int -> Int -> String
    renderRow cols r = [renderCell (r, c) | c <- [0 .. cols - 1]]
    renderCell :: Point -> Char
    renderCell p@(x, y)
      | p == s = 'S'
      | Set.member p v = '|'
      | otherwise = g !! x !! y

findStart :: [Row] -> Maybe Point
findStart [] = Nothing
findStart (firstRow : _) = (\j -> (0, j)) <$> elemIndex 'S' firstRow

filterPointsIn :: Board -> [Point] -> [Point]
filterPointsIn brd = filter (`isInside` brd)

get :: Int -> [a] -> Maybe a
get i list
  | i < 0 || i >= length list = Nothing
  | otherwise = Just $ list !! i -- still not a fan of !!

initialSplitState :: Board -> SplitState
initialSplitState brd =
  SplitState
    { sboard = brd,
      beams = Set.singleton (start brd),
      visited = Set.empty,
      splits = 0
    }

isInside :: Point -> Board -> Bool
isInside (x, y) brd =
  let (rows, cols) = dims brd
   in x >= 0 && x < rows && y >= 0 && y < cols

parseBoard :: [Row] -> Board
parseBoard [] = error "board is empty"
parseBoard g =
  if areLinesABoard g
    then case (\s -> Board {start = s, grid = g}) <$> findStart g of
      Just brd -> brd
      Nothing -> error "board is missing a starting position 'S'"
    else error "not a valid board: lines have different lengths"

parseFile :: FilePath -> IO Board
parseFile path = do
  contents <- readFile path
  let ls = lines contents
  pure $ parseBoard ls

row :: Int -> Board -> Maybe Row
row i Board {grid = g} = get i g
