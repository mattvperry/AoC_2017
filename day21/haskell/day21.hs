import Prelude hiding (lookup)
import Data.Map (Map, lookup, empty, insert)
import Data.Maybe (mapMaybe)
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)

type Image = [String]
type Rules = Map Image Image

parse :: String -> Rules
parse = foldl go empty . map (splitOn " => ") . lines
    where go m [l, r] = insert (splitOn "/" l) (splitOn "/" r) m

split :: Int -> [[a]] -> [[[a]]]
split s = concat . transpose . map (chunksOf s) . transpose . map (chunksOf s)

combine :: Int -> [[[a]]] -> [[a]]
combine s = transpose . append . map (transpose . append) . chunksOf s
    where append = foldl1 (zipWith (++))

transforms :: Image -> [Image]
transforms i = take 4 . iterate (reverse . transpose) =<< [i, reverse i]

step :: Rules -> Image -> Image
step rs i = head . combine (length i `div` size) . map (mapMaybe (`lookup` rs) . transforms) . split size $ i
    where size = 2 + (length i `mod` 2)

solve :: Rules -> Int -> Int
solve rs n = length . filter (== '#') . concat . (!! n) . iterate (step rs) $ [".#.", "..#", "###"]

main :: IO ()
main = do
    rules <- parse <$> readFile "day21/input.txt"
    print . solve rules $ 5
    print . solve rules $ 18