readGrid :: String -> IO [[Int]]
readGrid s = map (map read . words) . lines <$> readFile s

part1 :: [Int] -> Int
part1 = (-) <$> maximum <*> minimum

part2 :: [Int] -> Int
part2 xs = head [d | (d, 0) <- divMod <$> xs <*> xs, d /= 1]

main :: IO ()
main = do
    grid <- readGrid "day2/input.txt"
    print . sum . map part1 $ grid
    print . sum . map part2 $ grid