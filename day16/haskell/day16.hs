import Control.Arrow ((&&&))
import Data.Array.Unboxed (
    UArray
    , ixmap
    , bounds
    , elems
    , listArray
    , (//)
    , (!)
    )
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

type Programs = UArray Int Char;
data Move = Spin Int 
          | Exchange Int Int 
          | Partner Char Char 
          deriving (Show, Eq)

readMove :: String -> Move
readMove ('s':xs) = Spin (read xs)
readMove ('x':xs) = f . map read . splitOn "/" $ xs
    where f [a, b] = Exchange a b
readMove ('p':xs) = f . splitOn "/" $ xs
    where f [a:_, b:_] = Partner a b

parse :: String -> [Move]
parse = map readMove . splitOn ","

move :: Programs -> Move -> Programs
move ps (Spin n)       = ixmap (bounds ps) spin ps
    where spin i = abs $ (i - n) `mod` length (elems ps)
move ps (Exchange a b) = ps // [(a, ps ! b), (b, ps ! a)]
move ps (Partner a b)  = move ps $ Exchange (idx a) (idx b)
    where idx e = fromJust . elemIndex e . elems $ ps

makeMoves :: Programs -> [Move] -> Programs
makeMoves = foldl move

programs :: Programs
programs = listArray (0, 15) "abcdefghijklmnop"

part1 :: [Move] -> String
part1 = elems . makeMoves programs

findCycle :: [Move] -> [Programs]
findCycle ms = go [makeMoves programs ms]
    where go ps@(s:_)
            | s == programs = ps
            | otherwise     = go (makeMoves s ms : ps)

part2 :: [Move] -> String
part2 ms = elems $ c !! (l - 1000000000 `mod` l)
    where (l, c) = (length &&& id) (findCycle ms)

main :: IO ()
main = do
    input <- parse <$> readFile "day16/input.txt"
    print . part1 $ input
    print . part2 $ input