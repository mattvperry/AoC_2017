import Data.Map (Map, singleton)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Control.Lens

type Pos = (Int, Int)
type Grid = Map Pos Int

move :: Pos -> Int -> Pos
move (x, y) 0 = (x + 1, y)
move (x, y) 1 = (x, y - 1)
move (x, y) 2 = (x - 1, y)
move (x, y) 3 = (x, y + 1)
move (x, y) 4 = (x + 1, y + 1)
move (x, y) 5 = (x - 1, y - 1)
move (x, y) 6 = (x - 1, y + 1)
move (x, y) _ = (x + 1, y - 1)

spiral :: [Pos]
spiral = scanl move (0, 0) dirs
    where dirs = concat . zipWith replicate (concatMap (replicate 2) [1..]) $ cycle [0..3]

part1 :: Int -> Int
part1 = dist . (!!) spiral . pred
    where dist (x, y) = abs x + abs y

gridGen :: (Int, Grid) -> Pos -> (Int, Grid)
gridGen (_, g) p = (s, g & at p ?~ s)
    where s = sum . mapMaybe ((^.) g . at) . mapM (flip move) [0..7] $ p

part2 :: Int -> Maybe Int
part2 x = find (> x) $ map fst grid
    where grid = scanl gridGen (1, singleton (0, 0) 1) $ drop 1 spiral

main :: IO ()
main = do
    input <- readFile "day3/input.txt"
    print . part1 . read $ input
    print . part2 . read $ input