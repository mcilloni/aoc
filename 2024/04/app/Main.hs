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

pattern :: String
pattern = "XMAS"

data Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
    deriving (Bounded, Enum, Eq, Show)

directions :: [Direction]
directions = [minBound..maxBound]
    
dirVersor :: Direction -> (Int, Int)
dirVersor North = (0, 1)
dirVersor NorthEast = (1, 1)
dirVersor East = (1, 0)
dirVersor SouthEast = (1, -1)
dirVersor South = (0, -1)
dirVersor SouthWest = (-1, -1)
dirVersor West = (-1, 0)
dirVersor NorthWest = (-1, 1)

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

probeMatrix :: Eq a => Matrix a -> [a] -> Int
probeMatrix grid target = sum $ map (\x -> probePoint x grid target) $ coords grid

probePoint :: Eq a => (Int, Int) -> Matrix a -> [a] -> Int
probePoint (x, y) grid target = sum $ map (\dir -> fromEnum $ probePointTowards dir (x, y) grid target) directions 

probePointTowards :: Eq a => Direction -> (Int, Int) -> Matrix a -> [a] -> Bool
probePointTowards dir (x, y) grid target = case target of
    [] -> True
    (t:ts) -> case get2D (x, y) grid of
        Just c -> (c == t) && probePointTowards dir (moveTowards dir (x, y)) grid ts
        Nothing -> False