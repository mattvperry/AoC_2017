type Instruction = (String, Int, String, Int -> Int -> Bool, Int)

op :: String -> (Int -> Int -> Bool)
op ">"  = (>)
op "<"  = (<)
op ">=" = (>=)
op "<=" = (<=)
op "==" = (==)
op "!=" = (/=)

parse :: String -> Instruction
parse s = (r, if m == "int" then read m else -(read m), r', op o, read v)
    where [r, m, n, "if", r', o, v] = words s

main :: IO ()
main = do
    is <- map parse . lines <$> readFile "day8/input.txt"