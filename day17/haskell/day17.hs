import Control.Arrow (second)
import Data.List (splitAt, foldl')

positions :: Int -> [(Int, Int)]
positions n = scanl it (0, 0) [1..n]
    where it (_, c) a = (a, ((c + 369) `mod` a) + 1)

part1 :: Int -> Int
part1 n = (!! 1) . dropWhile (/= n) . foldl ins [] . positions $ n
    where ins xs (i, c) = uncurry (++) . second (i :) . splitAt c $ xs

part2 :: Int -> Int
part2 = foldl' go 0 . positions
    where go _ (i, 1) = i
          go v _      = v

main :: IO ()
main = do
    print . part1 $ 2017
    print . part2 $ 50000000