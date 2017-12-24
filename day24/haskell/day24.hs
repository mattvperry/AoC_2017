import Control.Arrow ((***), (&&&))
import Data.List.Split (splitOn)
import qualified Data.Set as S

type Component = (Int, Int)
type Bridge = [Component]

parse :: String -> S.Set Component
parse = S.fromList . map (f . splitOn "/") . lines
    where f [i, o] = (read i, read o)

matches :: Int -> S.Set Component -> S.Set Component
matches n = S.filter $ uncurry (||) . ((== n) *** (== n))

construct :: Int -> Bridge -> S.Set Component -> [Bridge]
construct n b cs
    | S.null ms = [b]
    | otherwise = concatMap go ms
    where ms          = matches n cs
          go m@(i, o) = construct (if n == i then o else i) (m:b) (S.delete m cs)

strongest :: [Bridge] -> Int
strongest = maximum . map (sum . map (uncurry (+)))

best :: [Bridge] -> Int
best ps = strongest . filter ((== len) . length) $ ps
    where len = maximum . map length $ ps

solve :: S.Set Component -> (Int, Int)
solve = (strongest &&& best) . construct 0 []

main :: IO ()
main = do
    cs <- parse <$> readFile "day24/input.txt"
    print . solve $ cs