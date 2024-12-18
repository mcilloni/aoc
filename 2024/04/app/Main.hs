-- AoC 2024 Day 4
-- Note: I am still not super good with Haskell, so I may have made some mistakes. Feel free to point them out.

module Main where
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input] -> do
            contents <- parseFile input
            let result = probeMatrix contents pattern
            print result
        _ -> do
            name <- getProgName
            hPutStr stderr $ "usage: " ++ name ++ " INPUT\n"
            exitFailure

data Direction = None | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
    deriving (Enum, Eq, Show)

dirVersor :: Direction -> (Int, Int)
dirVersor None = (0, 0)
dirVersor North = (0, 1)
dirVersor NorthEast = (1, 1)
dirVersor East = (1, 0)
dirVersor SouthEast = (1, -1)
dirVersor South = (0, -1)
dirVersor SouthWest = (-1, -1)
dirVersor West = (-1, 0)
dirVersor NorthWest = (-1, 1)

infixl 6 |+|
(|+|) :: (Int, Int) -> Direction -> (Int, Int)
(a, b) |+| dir = (a + dx, b + dy)
    where (dx, dy) = dirVersor dir

type Pattern a = (a, a, a)
pattern :: Pattern Char
pattern = ('M', 'A', 'S')

type AxisOffset = (Direction, Direction, Direction)

axisOffsets :: [[AxisOffset]]
axisOffsets =
    [[(NorthWest, None, SouthEast), (SouthEast, None, NorthWest)],
    [(SouthWest, None, NorthEast), (NorthEast, None, SouthWest)]]

newtype Matrix a = Matrix [[a]]

asMatrix :: [[a]] -> Matrix a
asMatrix [] = Matrix []
asMatrix m@(x:xs) = if all (\y -> length y == length x) xs then Matrix m else error "Not a matrix"

coords :: Matrix a -> [(Int, Int)]
coords (Matrix m) = [(x, y) | x <- [0..rows - 1], y <- [0..cols - 1]]
    where (rows, cols) = dim (Matrix m)

dim :: Matrix a -> (Int, Int)
dim (Matrix m) = case m of
    [] -> (0, 0)
    (x:_) -> (length m, length x)

unMatrix :: Matrix a -> [[a]]
unMatrix (Matrix m) = m

moveTowards :: Direction -> (Int, Int) -> (Int, Int)
moveTowards dir (x, y) = (x + dx, y + dy)
    where (dx, dy) = dirVersor dir

parseFile :: FilePath -> IO (Matrix Char)
parseFile path = do
    contents <- readFile path
    let ls = lines contents
    return $ asMatrix ls

get :: Int -> [a] -> Maybe a
get i list
    | i < 0 || i >= length list = Nothing
    | otherwise = Just $ list !! i -- not a fan of !!

get2D :: (Int, Int) -> Matrix a  -> Maybe a
get2D (x, y) grid = do
    row <- get y $ unMatrix grid
    get x row

probeMatrix :: Eq a => Matrix a -> Pattern a -> Int
probeMatrix grid target = sum $ map (\x -> fromEnum $ probePoint x grid target) $ coords grid

probePoint :: Eq a => (Int, Int) -> Matrix a -> Pattern a -> Bool
probePoint p grid target = all (any (probeOffSet p grid target)) axisOffsets

probeOffSet :: Eq a => (Int, Int) -> Matrix a -> Pattern a -> AxisOffset -> Bool
probeOffSet p grid (a, b, c) (o1, o2, o3) =
    case fetchAll grid [p |+| o1, p |+| o2, p |+| o3] of
        Just [x, y, z] -> (x, y, z) == (a, b, c)
        _ -> False

fetchAll :: Eq a => Matrix a -> [(Int, Int)] -> Maybe [a]
fetchAll grid points = allMaybes $ map (`get2D` grid) points

allMaybes :: [Maybe a] -> Maybe [a]
allMaybes = foldr (\x acc -> do
    xs <- acc
    x' <- x
    return $ x':xs) (Just [])
