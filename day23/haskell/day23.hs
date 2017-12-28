{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import Data.Char (isLetter)
import Data.Map (Map, empty)
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Sequence as S
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.State (State, get, put, evalState)

type Param = Either Char Int
type Registers = Map Char Int

data Op
    = Set Char Param
    | Sub Char Param
    | Mul Char Param
    | Jnz Param Param
      deriving (Eq, Show)

data Status 
    = Terminated
    | Running
      deriving (Eq, Show)

data Program = Program
    { _pc     :: Int
    , _regs   :: Registers
    , _status :: Status
    , _code   :: S.Seq Op
    } deriving (Show)
makeLenses ''Program

parse :: String -> S.Seq Op
parse = S.fromList . mapMaybe (go . words) . lines
    where go ["set", x, y] = Just $ Set (head x) (f y)
          go ["sub", x, y] = Just $ Sub (head x) (f y)
          go ["mul", x, y] = Just $ Mul (head x) (f y)
          go ["jnz", x, y] = Just $ Jnz (f x) (f y)
          go _             = Nothing
          f x
            | all isLetter x = Left $ head x
            | otherwise      = Right $ read x

reg :: Char -> Lens' Program Int
reg r = regs . at r . non 0

val :: Param -> Program -> Int
val = either (view . reg) const

perform :: Program -> Op -> Program
perform p (Set x y) = p & reg x .~ (val y p)
perform p (Sub x y) = p & reg x -~ (val y p)
perform p (Mul x y) = p & reg x *~ (val y p)
perform p (Jnz x y)
    | val x p /= 0  = p & pc +~ (val y p - 1)
    | otherwise     = p

step :: Program -> (Program, Maybe Op)
step p = (go &&& id) $ p ^? code . ix (p ^. pc)
    where go (Just o) = (perform p o) & pc +~ 1
          go Nothing  = p & status .~ Terminated

exec :: State Program [Maybe Op]
exec = do
    (p, o) <- step <$> get
    put p
    case view status p of
        Running    -> return . (o :) =<< exec
        Terminated -> return []

part1 :: S.Seq Op -> Int
part1 c = length . filter f . catMaybes . evalState exec $ p
    where p = Program { _pc = 0, _regs = empty, _status = Running, _code = c }
          f (Mul _ _) = True
          f _         = False

part2 :: Int
part2 = length . filter (not . isPrime) $ [108100,108117..125100]
    where isqrt = floor . sqrt . fromIntegral
          isPrime k = null [ x | x <- [2..isqrt k], k `mod` x == 0 ]

main :: IO ()
main = do
    input <- parse <$> readFile "day23/input.txt"
    print . part1 $ input
    print part2