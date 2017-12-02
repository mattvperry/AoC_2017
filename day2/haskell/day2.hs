import Control.Arrow ((&&&))
import Data.List (subsequences)
import Data.List.Split (splitOn)
import Data.Tuple (swap)

readGrid :: String -> IO [[Int]]
readGrid s = map (map read . splitOn "\t") . lines <$> readFile s

seqPairs :: [a] -> [(a, a)]
seqPairs = map fn . filter ((== 2) . length) . subsequences
    where fn [a, b] = (a, b)

allPairs :: [a] -> [(a, a)]
allPairs xs = ps ++ map swap ps
    where ps = seqPairs xs

mapSum :: Num b => (a -> b) -> [a] -> b
mapSum f = sum . map f

part1 :: [Int] -> Int
part1 = uncurry subtract . (minimum &&& maximum)

part2 :: [Int] -> Int
part2 = fst . head . filter ((== 0) . snd) . map (uncurry divMod) . allPairs

main :: IO ()
main = do
    grid <- readGrid "day2/input.txt"
    print . mapSum part1 $ grid
    print . mapSum part2 $ grid