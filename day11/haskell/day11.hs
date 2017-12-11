import Control.Arrow ((&&&))

type Coord = (Int, Int, Int)

move :: Coord -> String -> Coord
move (x, y, z) "n"  = (x - 1, y + 1, z)
move (x, y, z) "ne" = (x, y + 1, z - 1)
move (x, y, z) "nw" = (x - 1, y, z + 1)
move (x, y, z) "s"  = (x + 1, y - 1, z)
move (x, y, z) "se" = (x + 1, y, z - 1)
move (x, y, z) "sw" = (x, y - 1, z + 1)

parse :: String -> [String]
parse = words . map f
    where f ',' = ' '
          f x   = x

solve :: [String] -> (Int, Int)
solve = (last &&& maximum) . map f . scanl move (0, 0, 0)
    where f (x, y, z) = maximum [abs x, abs y, abs z]
        
main :: IO ()
main = do
    input <- parse <$> readFile "day11/input.txt"
    print . solve $ input