import              Control.Arrow   ((***))
import qualified    Data.Map as M
import              Data.Maybe      (mapMaybe)

type Pos = (Int, Int)
type Grid = M.Map Pos Int

move :: Pos -> Int -> Pos
move (x, y) 0 = (x, y - 1)
move (x, y) 1 = (x - 1, y)
move (x, y) 2 = (x, y + 1)
move (x, y) 3 = (x + 1, y)
move (x, y) 4 = (x + 1, y + 1)
move (x, y) 5 = (x - 1, y - 1)
move (x, y) 6 = (x - 1, y + 1)
move (x, y) _ = (x + 1, y - 1)

spiral :: [Pos]
spiral = scanl move (0, 1) dirs
    where dirs = concat . zipWith replicate (concatMap (replicate 2) [1..]) $ cycle [0..3]

part1 :: Int -> Int
part1 = uncurry (+) . (abs *** abs) . (!!) spiral

gridGen :: (Int, Grid) -> Pos -> (Int, Grid)
gridGen (_, g) p = (s, M.insert p s g)
    where s = sum . mapMaybe (`M.lookup` g) . mapM (flip move) [0..7] $ p

part2 :: Int -> Int
part2 x = head . dropWhile (<= x) $ map fst grid
    where grid = scanl gridGen (1, M.singleton (0, 0) 1) spiral

main :: IO ()
main = do
    input <- read <$> readFile "day3/input.txt"
    print . part1 $ input
    print . part2 $ input