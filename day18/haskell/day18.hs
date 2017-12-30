{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

import Data.Char (isAlpha)
import Data.Map (Map, empty)
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State (State, execState)

type Param = Either Char Int
type Registers = Map Char Int

data Op
    = Snd Param
    | Set Char Param
    | Bin (Int -> Int -> Int) Char Param
    | Rcv Param
    | Jgz Param Param

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
    , _code   :: V.Vector Op
    }
makeLenses ''Program

parse :: String -> V.Vector Op
parse = V.fromList . mapMaybe (go . words) . lines
    where go ["snd", x]     = Just $ Snd (f x)
          go ["set", x, y]  = Just $ Set (head x) (f y)
          go ["add", x, y]  = Just $ Bin (+) (head x) (f y)
          go ["mul", x, y]  = Just $ Bin (*) (head x) (f y)
          go ["mod", x, y]  = Just $ Bin mod (head x) (f y)
          go ["rcv", x]     = Just $ Rcv (f x)
          go ["jgz", x, y]  = Just $ Jgz (f x) (f y)
          go _              = Nothing
          f [c] | isAlpha c = Left c
          f str             = Right $ read str

reg :: Char -> Lens' Program Int
reg r = regs . at r . non 0

val :: Param -> State Program Int
val = either (use . reg) pure

perform :: Op -> State Program ()
perform (Snd x)     = (<|)   <$> val x >>= modifying sounds
perform (Set x y)   =            val y >>= assign (reg x)
perform (Bin f x y) = flip f <$> val y >>= modifying (reg x)
perform (Rcv x)     = (/= 0) <$> val x >>= flip when (status .= Terminated)
perform (Jgz x y)   = (>  0) <$> val x >>= flip when (modifying pc =<< ((+) . pred <$> val y))

step :: State Program ()
step = use pc >>= (\n -> preuse (code . ix n)) >>= \case
    Just x  -> perform x >> pc += 1
    Nothing -> status .= Terminated

exec :: State Program ()
exec = step >> use status >>= (\s -> unless (s == Terminated) exec)

part1 :: V.Vector Op -> Maybe Int
part1 c = execState exec p ^? sounds . ix 0
    where p = Program { _pc = 0, _regs = empty, _sounds = [], _status = Running, _code = c }

main :: IO ()
main = do
    input <- parse <$> readFile "day18/input.txt"
    print . part1 $ input