import Control.Lens (over, set, ix)
import Data.Array (Array, listArray, assocs)
import Control.Arrow (second)
import Data.List (maximumBy)
import Data.Ord (comparing, Down(..))
import Data.Set (Set, member, insert, empty)
import Data.Tuple (swap)

type Banks = Array Int Int

parse :: String -> Banks
parse s = listArray (0, length l - 1) l
    where l = map read . words $ s

redist :: Banks -> Banks
redist b = foldl (flip ($)) (set (ix i) 0 b) $ map f [i + 1..i + v]
    where f = flip over succ . ix . (`mod` length b)
          (i, v) = maximumBy (comparing $ second Down . swap) . assocs $ b

findDupe :: Set Banks -> Int -> Banks -> (Int, Banks)
findDupe s c b
    | member b s = (c, b)
    | otherwise  = findDupe (insert b s) (c + 1) (redist b)

part1 :: Banks -> Int
part1 = fst . findDupe empty 0

part2 :: Banks -> Int
part2 = fst . findDupe empty 0 . snd . findDupe empty 0

main :: IO ()
main = do
    banks <- parse <$> readFile "day6/input.txt"
    print . part1 $ banks
    print . part2 $ banks