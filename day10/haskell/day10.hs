import Numeric (showHex)
import Data.Bits (xor)
import Data.Char (ord)
import Control.Arrow ((***))

roll :: Int -> Int -> [Int] -> [Int]
roll p l xs = end ++ start
    where len           = length xs
          (knot, keep)  = splitAt l . drop p $ xs ++ xs
          (start, end)  = splitAt (len - p) . take len $ reverse knot ++ keep

sparse :: Int -> Int -> [Int] -> [Int] -> (Int, Int, [Int])
sparse p s l []     = (p, s, l)
sparse p s l (x:xs) = sparse next (s + 1) (roll p x l) xs
    where next = (p + x + s) `mod` length l

parse1 :: String -> [Int]
parse1 = map read . words . map f
    where f ',' = ' '
          f x   = x

part1 :: String -> Int
part1 = f . sparse 0 0 [0..255] . parse1
    where f (_, _, x:y:_) = x * y

parse2 :: String -> [Int]
parse2 = (++ [17, 31, 73, 47, 23]) . map ord

loop :: [Int] -> [(Int, Int, [Int])]
loop = scanl f (0, 0, [0..255]) . repeat
    where f (p, s, l) = sparse p s l

compress :: [Int] -> String
compress [] = []
compress xs = uncurry (++) . (hex *** compress) . splitAt 16 $ xs
    where hex = flip showHex [] . foldl1 xor

part2 :: String -> String
part2 i = compress h
    where (_, _, h) = (!! 64) . loop . parse2 $ i

main :: IO ()
main = do
    input <- readFile "day10/input.txt"
    print . part1 $ input
    print . part2 $ input