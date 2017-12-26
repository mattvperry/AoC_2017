import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST (ST, runST)

parse :: String -> V.Vector Int
parse = V.fromList . map read . lines

run :: (Int -> Int) -> Int -> Int -> VM.MVector s Int -> ST s Int
run f n i v
    | i < 0 || i >= VM.length v = return n
    | otherwise = do
        j <- VM.read v i
        VM.modify v f i
        run f (n + 1) (i + j) v

part1 :: V.Vector Int -> Int
part1 os = runST $ V.thaw os >>= run (+1) 0 0

part2 :: V.Vector Int -> Int
part2 os = runST $ V.thaw os >>= run f 0 0
    where f x = if x >= 3 then x - 1 else x + 1

main :: IO ()
main = do
    offsets <- parse <$> readFile "day5/input.txt"
    print . part1 $ offsets
    print . part2 $ offsets