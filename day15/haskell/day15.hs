import Data.Bits ((.&.))

type Generator = (Int -> Bool) -> [Int]

generate :: Int -> Int -> (Int -> Bool) -> [Int]
generate v f c = map (.&. 0xFFFF) . filter c . iterate next $ v
    where next = flip mod 2147483647 . (* f)

genA :: Generator
genA = generate 883 16807

genB :: Generator
genB = generate 879 48271

solve :: [Int] -> [Int] -> Int -> Int
solve a b n = length . filter id . take n $ zipWith (==) a b

part1 :: Int
part1 = solve (genA $ const True) (genB $ const True) 40000000

part2 :: Int
part2 = solve (genA $ f 4) (genB $ f 8) 5000000
    where f c = (== 0) . flip mod c

main :: IO ()
main = do
    print part1
    print part2