import Data.IntMap (IntMap)
import Data.Map (Map, (!))
import qualified Data.IntMap as I
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Value = Z | O deriving (Eq, Show, Ord)

data Turing = Turing
    { state :: Char
    , cursor :: Int
    , tape :: IntMap Value
    } deriving (Show)

data Move = Move
    { set :: Value
    , change :: Int
    , next :: Char
    } deriving (Show)

states :: Map (Char, Value) Move
states = M.fromList 
    [ (('A', Z), Move { set = O, change = 1, next = 'B' })
    , (('A', O), Move { set = Z, change = -1, next = 'E' })
    , (('B', Z), Move { set = O, change = -1, next = 'C' })
    , (('B', O), Move { set = Z, change = 1, next = 'A' })
    , (('C', Z), Move { set = O, change = -1, next = 'D' })
    , (('C', O), Move { set = Z, change = 1, next = 'C' })
    , (('D', Z), Move { set = O, change = -1, next = 'E' })
    , (('D', O), Move { set = Z, change = -1, next = 'F' })
    , (('E', Z), Move { set = O, change = -1, next = 'A' })
    , (('E', O), Move { set = O, change = -1, next = 'C' })
    , (('F', Z), Move { set = O, change = -1, next = 'E' })
    , (('F', O), Move { set = O, change = 1, next = 'A' })]

step :: Turing -> Turing
step (Turing { state = s, cursor = c, tape = t }) = Turing 
    { state = next move
    , tape = I.insert c (set move) t
    , cursor = c + change move }
    where move = states ! (s, fromMaybe Z $ I.lookup c t)

part1 :: Int
part1 = length . filter (== O) . I.elems . tape . (!! 12208951) . iterate step $ start
    where start = Turing { state = 'A', cursor = 0, tape = I.empty }

main :: IO ()
main = print part1