import Data.Bits ((.&.))

type Generator = (Int -> Bool) -> [Int]

generate :: Int -> Int -> (Int -> Bool) -> [Int]
generate v f c = map (.&. 0xFFFF) . filter c . iterate next $ v
    where next = flip mod 2147483647 . (* f)

solve :: [Int] -> [Int] -> Int -> Int
solve a b n = length . filter id . take n $ zipWith (==) a b

part1 :: Generator -> Generator -> Int
part1 a b = solve (a $ const True) (b $ const True) 40000000

part2 :: Generator -> Generator -> Int
part2 a b = solve (a $ f 4) (b $ f 8) 5000000
    where f c = (== 0) . flip mod c

main :: IO ()
main = do
    let a = generate 883 16807
    let b = generate 879 48271
    print $ part1 a b
    print $ part2 a b