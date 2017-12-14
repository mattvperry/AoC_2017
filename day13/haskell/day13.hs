import Data.Char (isDigit)
import Data.List (findIndex)

type Layer = (Int, Int, Int)
type Firewall = [Layer]

parse :: String -> Firewall
parse = map (f . map (read . filter isDigit) . words) . lines
    where f [d, r] = (d, r, 0)

stepper :: Int -> Firewall -> Firewall
stepper i = map step
    where step (d, r, s) = (d, r, (s + d + i) `mod` (r * 2 - 2))

hit :: Layer -> Bool
hit (_, _, s) = s == 0

part1 :: Firewall -> Int
part1 = sum . map sev . filter hit . stepper 0
    where sev (d, r, _) = d * r

part2 :: Firewall -> Maybe Int
part2 f = fmap (+ 1) . findIndex (not . any hit . ($ f)) . map stepper $ [1..]

main :: IO ()
main = do
    firewall <- parse <$> readFile "day13/input.txt"
    print . part1 $ firewall
    print . part2 $ firewall