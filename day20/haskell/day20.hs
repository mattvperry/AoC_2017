import Data.List.Split (splitOn)
import Data.List (minimumBy, groupBy)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Function (on)

type Vector = (Int, Int, Int)
type Particle = (Vector, Vector, Vector)

instance Monoid Int where
    mempty = 0
    mappend = (+)

parse :: String -> Particle
parse = g . map f . concatMap (drop 1 . splitOn "=") . splitOn ", "
    where f = g . map read . splitOn "," . init . tail
          g [a, b, c] = (a, b, c)

part1 :: [Particle] -> Int
part1 = fst . minimumBy (comparing snd) . zipWith f [0..]
    where f i (_, _, (x, y, z)) = (i, abs x + abs y + abs z)

move :: Particle -> Particle
move (p, v, a) = (p <> v <> a, v <> a, a) 

step :: [Particle] -> [Particle]
step = map move . concat . filter ((== 1) . length) . groupBy ((==) `on` pos)
    where pos (p, _, _) = p

part2 :: [Particle] -> Int
part2 = length . (!! 50) . iterate step

main :: IO ()
main = do
    ps <- map parse . lines <$> readFile "day20/input.txt"
    print . part1 $ ps
    print . part2 $ ps