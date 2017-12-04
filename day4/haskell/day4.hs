import Data.List (nub, sort)

part1 :: Eq a => [[a]] -> Int
part1 = length . filter uniq
    where uniq = (==) <$> length <*> length . nub

part2 :: Ord a => [[[a]]] -> Int
part2 = part1 . map (map sort)

main :: IO ()
main = do
    input <- map words . lines <$> readFile "day4/input.txt"
    print . part1 $ input
    print . part2 $ input