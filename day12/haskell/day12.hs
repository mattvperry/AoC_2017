import Prelude hiding (lookup)
import Data.Char (isDigit)
import Data.List ((\\))
import Data.Map (Map, fromListWith, lookup, keys)
import Data.Maybe (fromMaybe)
import Data.Set (Set, empty, member, insert, toList)

type Graph = Map String [String]

parse :: String -> Graph
parse = fromListWith (++) . map (f . words) . lines
    where f (x:"<->":xs) = (x, map (filter isDigit) xs)

group :: String -> Graph -> Set String
group n g = go [n] empty
    where go [] s = s
          go (v:vs) s
            | member v s = go vs s
            | otherwise  = go (vs ++ fromMaybe [] (lookup v g)) (insert v s)

groups :: Int -> Graph -> [String] -> Int
groups c _ []     = c
groups c g (n:ns) = groups (c + 1) g (ns \\ toList (group n g))

part1 :: Graph -> Int
part1 = length . group "0"

part2 :: Graph -> Int
part2 g = groups 0 g (keys g)

main :: IO ()
main = do
    graph <- parse <$> readFile "day12/input.txt"
    print . part1 $ graph
    print . part2 $ graph