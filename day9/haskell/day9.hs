solve :: Bool -> Int -> Int -> Int -> String -> (Int, Int)
solve _ _ s n []    = (s, n)
solve g d s n (c:xs)
    | c == '!'      = solve g d s n (tail xs)
    | c /= '>' && g = solve g d s (n + 1) xs
    | c == '<'      = solve True d s n xs
    | c == '>'      = solve False d s n xs
    | c == '{'      = solve g (d + 1) (s + d + 1) n xs
    | c == '}'      = solve g (d - 1) s n xs
    | otherwise     = solve g d s n xs

main :: IO ()
main = do
    input <- readFile "day9/input.txt"
    print . solve False 0 0 0 $ input