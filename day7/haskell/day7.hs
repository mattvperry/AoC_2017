import Control.Arrow ((&&&))
import Control.Monad (when)
import Control.Monad.State (State, get, put, execState)
import Data.List (find, elem, groupBy, sortOn)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, elems, (!))
import Data.Maybe (fromJust, isNothing)
import Data.Function (on)

data Node = Node 
    { name :: String
    , weight :: Int
    , children :: [String] 
    } deriving (Eq, Show)

type Tree = Map String Node

parse :: String -> Tree
parse = fromList . map ((name &&& id) . f . words) . lines
    where f (n:w:[])   = Node { name = n, weight = g w, children = [] }
          f (n:w:_:ns) = Node { name = n, weight = g w, children = splitOn "," . concat $ ns }
          g = read . init . tail

root :: Tree -> Node
root t = go . fromJust . find (null . children) $ vs
    where vs = elems t
          go n
            | Just n' <- find (elem (name n) . children) vs = go n'
            | otherwise = n

analyze :: [(Int, String)] -> Maybe (Int, String)
analyze xs
    | length gs == 2 = Just (g - b, n)
    | otherwise      = Nothing
    where gs = groupBy ((==) `on` fst) xs
          [(b,n),(g,_)] = map head . sortOn length $ gs

weigh :: Tree -> String -> State (Maybe Int) (Int, String)
weigh t n = do
    let Node { children = cs, weight = w } = t ! n
    ws <- mapM (weigh t) cs
    notFound <- isNothing <$> get
    when notFound $ case analyze ws of
        Just (d, n') -> put . Just $ (weight $ t ! n') + d
        Nothing      -> return ()
    return (w + sum (map fst ws), n)

part1 :: Tree -> String
part1 = name . root

part2 :: Tree -> Maybe Int
part2 t = execState (weigh t . name . root $ t) Nothing

main :: IO ()
main = do
    tree <- parse <$> readFile "day7/input.txt"
    print . part1 $ tree
    print . part2 $ tree