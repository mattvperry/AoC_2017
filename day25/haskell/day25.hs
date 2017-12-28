import Data.Set (Set, notMember, insert, delete, empty, size)

data State = A | B | C | D | E | F deriving (Eq)

step :: Int -> State -> Int -> Set Int -> Set Int
step 0 _ _ s        = s
step n A c s
    | notMember c s = step (n - 1) B (c + 1) (insert c s)
    | otherwise     = step (n - 1) E (c - 1) (delete c s)
step n B c s
    | notMember c s = step (n - 1) C (c - 1) (insert c s)
    | otherwise     = step (n - 1) A (c + 1) (delete c s)
step n C c s
    | notMember c s = step (n - 1) D (c - 1) (insert c s)
    | otherwise     = step (n - 1) C (c + 1) (delete c s)
step n D c s
    | notMember c s = step (n - 1) E (c - 1) (insert c s)
    | otherwise     = step (n - 1) F (c - 1) (delete c s)
step n E c s
    | notMember c s = step (n - 1) A (c - 1) (insert c s)
    | otherwise     = step (n - 1) C (c - 1) s
step n F c s
    | notMember c s = step (n - 1) E (c - 1) (insert c s)
    | otherwise     = step (n - 1) A (c + 1) s

part1 :: Int
part1 = size $ step 12208951 A 0 empty

main :: IO ()
main = print part1