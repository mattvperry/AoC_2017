import Control.Arrow (first)
import Data.Map (Map, findWithDefault, empty, insert)

type State = (Map String Int, Int)
type Instruction = (String, Int, String, Int -> Bool)

op :: String -> (Int -> Int -> Bool)
op ">"  = (<)
op "<"  = (>)
op ">=" = (<=)
op "<=" = (>=)
op "==" = (==)
op "!=" = (/=)
op _    = error "Unknown operation"

parse :: String -> Instruction
parse s = (r, if m == "inc" then read n else -(read n), r', op o (read v))
    where [r, m, n, "if", r', o, v] = words s

modify :: State -> Instruction -> State
modify s@(rs, m) (r, a, r', c)
        | not . cond $ rs   = s
        | otherwise         = (insert r val rs, max m val)
    where cond = c . findWithDefault 0 r'
          val  = findWithDefault 0 r rs + a

solve :: [Instruction] -> (Int, Int)
solve = first maximum . foldl modify (empty, 0)

main :: IO ()
main = do
    is <- map parse . lines <$> readFile "day8/input.txt"
    print . solve $ is