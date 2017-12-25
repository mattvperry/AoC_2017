{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import Data.Char (isLetter)
import Data.Map (Map, empty)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as S
import Control.Lens
import Control.Monad (unless)
import Control.Monad.State (State, modify, gets, runState)

type Param = Either Char Int
type Registers = Map Char Int

data Op
    = Snd Param
    | Set Char Param
    | Add Char Param
    | Mul Char Param
    | Mod Char Param
    | Rcv Param
    | Jgz Param Param
      deriving (Eq, Show)

data Status 
    = Terminated
    | Waiting
    | Running
      deriving (Eq, Show)

data Program = Program
    { _pc     :: Int
    , _regs   :: Registers
    , _sounds :: [Int]
    , _status :: Status
    , _code   :: S.Seq Op
    } deriving (Show)
makeLenses ''Program

parse :: String -> S.Seq Op
parse = S.fromList . mapMaybe (go . words) . lines
    where go ["snd", x]    = Just $ Snd (f x)
          go ["set", x, y] = Just $ Set (head x) (f y)
          go ["add", x, y] = Just $ Add (head x) (f y)
          go ["mul", x, y] = Just $ Mul (head x) (f y)
          go ["mod", x, y] = Just $ Mod (head x) (f y)
          go ["rcv", x]    = Just $ Rcv (f x)
          go ["jgz", x, y] = Just $ Jgz (f x) (f y)
          go _             = Nothing
          f x
            | all isLetter x = Left $ head x
            | otherwise      = Right $ read x

reg :: Char -> Lens' Program Int
reg r = regs . at r . non 0

val :: Param -> Program -> Int
val = either (view . reg) const

perform :: Program -> Op -> Program
perform p (Snd x)   = p & sounds %~ ((val x p) <|)
perform p (Set x y) = p & reg x .~ (val y p)
perform p (Add x y) = p & reg x +~ (val y p)
perform p (Mul x y) = p & reg x *~ (val y p)
perform p (Mod x y) = p & reg x %~ (`mod` (val y p))
perform p (Rcv x)
    | val x p /= 0 = p & status .~ Terminated 
    | otherwise    = p
perform p (Jgz x y)
    | val x p > 0  = p & pc +~ (val y p - 1)
    | otherwise    = p

step :: Program -> Program
step p = go $ p ^? code . ix (p ^. pc)
    where go (Just o) = (perform p o) & pc +~ 1
          go Nothing  = p & status .~ Terminated

exec :: State Program ()
exec = do
    modify step
    s <- gets (view status)
    unless (s == Terminated) exec

part1 :: S.Seq Op -> Maybe Int
part1 c = (snd $ runState exec p) ^? sounds . ix 0
    where p = Program { _pc = 0, _regs = empty, _sounds = [], _status = Running, _code = c }

main :: IO ()
main = do
    input <- parse <$> readFile "day18/input.txt"
    print . part1 $ input