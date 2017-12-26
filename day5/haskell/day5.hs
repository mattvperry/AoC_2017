import Control.Lens
import Data.Array (Array, listArray)

type Tape = Array Int Int

parse :: String -> Tape
parse i = listArray (0, length offsets - 1) offsets
    where offsets = map read . lines $ i

jump :: (Int -> Int) -> Int -> Int -> Tape -> Int
jump f n i os
    | Just v <- val = jump f (n + 1) (i + v) (os & ix i +~ f v)
    | otherwise = n
    where val = os ^? ix i

part1 :: Tape -> Int
part1 = jump (const 1) 0 0

part2 :: Tape -> Int
part2 = jump f 0 0
    where f x = if x >= 3 then -1 else 1

main :: IO ()
main = do
    offsets <- parse <$> readFile "day5/input.txt"
    print . part1 $ offsets
    print . part2 $ offsets