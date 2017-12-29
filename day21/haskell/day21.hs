{-# LANGUAGE RankNTypes #-}

import Prelude hiding (lookup)
import Control.Lens (over, Traversal')
import Data.Map (Map, (!), fromList)
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)

type Image = [String]
type Rules = Map Image Image

transforms :: Image -> [Image]
transforms i = take 4 . iterate (reverse . transpose) =<< [i, reverse i]

parse :: String -> Rules
parse = fromList . concatMap (f . map (splitOn "/") . splitOn " => ") . lines
    where f [l, r] = zip (transforms l) (repeat r)

split :: Int -> Image -> [[Image]]
split s = transpose . map (map transpose . chunksOf s . transpose) . chunksOf s

combine :: [[Image]] -> Image
combine = transpose . concatMap (transpose . concat)

subGrids :: Int -> Traversal' Image Image
subGrids n f = fmap combine . (traverse . traverse) f . split n

step :: Rules -> Image -> Image
step rs i = over (subGrids size) (rs !) i
    where size = 2 + (length i `mod` 2)

solve :: Rules -> Int -> Int
solve rs n = length . filter (== '#') . concat . (!! n) . iterate (step rs) $ [".#.", "..#", "###"]

main :: IO ()
main = do
    rules <- parse <$> readFile "day21/input.txt"
    print . solve rules $ 5
    print . solve rules $ 18