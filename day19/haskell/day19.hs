import Control.Arrow ((&&&))
import Data.Char (isAlpha)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Map (Map, (!), fromList)

type Coord = (Int, Int)
type Grid = Map Coord Char

data Dir = U | D | L | R deriving (Show, Eq)

move :: Coord -> Dir -> Coord
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)
move (x, y) R = (x + 1, y)

rev :: Dir -> Dir
rev U = D
rev D = U
rev L = R
rev R = L

parse :: String -> Grid
parse = fromList . concat . zipWith f [0..] . map (zip [0..]) . lines
    where f y = map $ g y
          g y (x, c) = ((x, y), c)

start :: Grid -> Coord
start g = fromJust . find ((== '|') . (g !)) . map (id &&& const 0) $ [0..]

turn :: Grid -> Coord -> Dir -> Dir
turn g c d = fromJust . find ((/= ' ') . (g !) . move c) . filter (/= rev d) $ [U, D, L, R]

walk :: Grid -> String -> Dir -> Coord -> String
walk g p d c
    | val == ' ' = p
    | val == '+' = walk g (val:p) next (move c next)
    | otherwise  = walk g (val:p) d (move c d)
    where (val, next) = (g ! c, turn g c d)

solve :: Grid -> (String, Int)
solve g = (reverse . filter isAlpha &&& length) $ walk g [] D (start g)

main :: IO ()
main = do
    grid <- parse <$> readFile "day19/input.txt"
    print . solve $ grid