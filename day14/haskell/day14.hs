import qualified Day10.Haskell.Day10 as D10
import qualified Day12.Haskell.Day12 as D12

import Numeric (showIntAtBase, showInt)
import Control.Arrow (first)
import Data.Char (intToDigit, digitToInt)
import qualified Data.Map as M
import qualified Data.Set as S

type Grid = [Int]
type Coord = (Int, Int)
type Graph = M.Map Coord [Coord]

binary :: Char -> [Int]
binary s = map digitToInt . D10.lpad 4 '0' $ showIntAtBase 2 intToDigit n []
    where n = digitToInt s

makeGrid :: String -> Grid
makeGrid s = concatMap f [0..127]
    where f = concatMap binary . D10.part2 . ((s ++ "-") ++) . flip showInt []

part1 :: Grid -> Int
part1 = sum

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)] 

occupied :: Grid -> [Coord]
occupied = map fst . filter ((== 1) . snd) . map (first (`divMod` 128)) . zip [0..]

makeGraph :: S.Set Coord -> Graph
makeGraph cs = foldl f M.empty cs
    where f a c = M.insert c (filter (`S.member` cs) (neighbors c)) a

part2 :: Grid -> Int
part2 = D12.part2 . makeGraph . S.fromList . occupied

main :: IO ()
main = do
    grid <- makeGrid <$> readFile "day14/input.txt"
    print . part1 $ grid
    print . part2 $ grid