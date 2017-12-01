rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

sumPairs :: Int -> [Int] -> Int
sumPairs d xs = sum . zipWith fn (rotate d xs) $ xs
    where fn a b = if a == b then a else 0

solve :: Int -> String -> Int
solve d = sumPairs d . map (read . pure)

main :: IO ()
main = do
    input <- readFile "day1/input.txt"
    print . solve 1 $ input
    print . solve (length input `div` 2) $ input