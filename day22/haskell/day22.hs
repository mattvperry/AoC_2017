{-# LANGUAGE LambdaCase #-}

import Data.Map (Map, findWithDefault, insert, fromList)

data Cell = I | C | W | F deriving (Eq, Show)
type Coord = (Int, Int)
type Grid = Map Coord Cell

parse :: String -> Grid
parse = fromList . concatMap f . zip [0..] . map (zip [0..]) . lines
    where f (y, xs)  = map (g y) xs
          g y (x, c) = ((x, y), if c == '#' then I else C)

move :: Int -> Coord -> Coord
move 0 (x, y) = (x, y - 1)
move 1 (x, y) = (x + 1, y)
move 2 (x, y) = (x, y + 1)
move _ (x, y) = (x - 1, y)

travel :: (Cell -> (Int, Cell)) -> Int -> Int -> Coord -> Grid -> Int -> Int
travel _ 0 _ _ _ i = i
travel f n d c g i
    | s == I    = next (i + 1)
    | otherwise = next i
    where (t, s) = f (findWithDefault C c g)
          dir    = ((d + t) `mod` 4)
          next   = travel f (n - 1) dir (move dir c) (insert c s g)

part1 :: Grid -> Int
part1 g = travel f 10000 0 (12, 12) g 0 
    where f = \case
                I -> (1, C)
                _ -> (-1, I)

part2 :: Grid -> Int
part2 g = travel f 10000000 0 (12, 12) g 0 
    where f = \case
                I -> (1, F)
                W -> (0, I)
                F -> (2, C)
                C -> (-1, W)

main :: IO ()
main = do
    grid <- parse <$> readFile "day22/input.txt"
    print . part1 $ grid
    print . part2 $ grid