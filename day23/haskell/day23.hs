{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import Data.Char (isAlpha)
import Data.Map (Map, empty)
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(..))
import qualified Data.Vector as V
import Control.Applicative (many)
import Control.Lens
import Control.Monad (when)
import Control.Monad.State (StateT, execStateT, get)
import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

type Param = Either Char Int

data Op
    = Set Char Param
    | Sub Char Param
    | Mul Char Param
    | Jnz Param Param
      deriving (Eq, Show)

data Program = Program
    { _pc     :: Int
    , _regs   :: Map Char Int
    , _code   :: V.Vector Op
    } deriving (Show)
makeLenses ''Program

type App = MaybeT (StateT Program (Writer (Sum Int)))

parse :: String -> V.Vector Op
parse = V.fromList . mapMaybe (go . words) . lines
    where go ["set", x, y]  = Just $ Set (head x) (f y)
          go ["sub", x, y]  = Just $ Sub (head x) (f y)
          go ["mul", x, y]  = Just $ Mul (head x) (f y)
          go ["jnz", x, y]  = Just $ Jnz (f x) (f y)
          go _              = Nothing
          f [c] | isAlpha c = Left c
          f str             = Right $ read str

reg :: Char -> Lens' Program Int
reg r = regs . at r . non 0

val :: Param -> App Int
val = either (use . reg) pure

perform :: Op -> App ()
perform (Set x y) = (reg x .=) =<< val y
perform (Sub x y) = (reg x -=) =<< val y
perform (Mul x y) = (reg x *=) =<< val y
perform (Jnz x y) = (/= 0) <$> val x >>= flip when ((pc +=) =<< pred <$> val y)

step :: App ()
step = do
    c <- use pc
    Just o <- preuse (code . ix c)
    when (isMul o) (tell . Sum $ 1)
    perform o >> pc += 1
    where isMul (Mul _ _) = True
          isMul _         = False

execApp :: App a -> Program -> Writer (Sum Int) Program
execApp = execStateT . runMaybeT

part1 :: V.Vector Op -> Int
part1 = getSum . execWriter . execApp (many step) . Program 0 empty

part2 :: Int
part2 = length . filter (not . isPrime) $ [108100,108117..125100]
    where isqrt = floor . sqrt . fromIntegral
          isPrime k = null [ x | x <- [2..isqrt k], k `mod` x == 0 ]

main :: IO ()
main = do
    input <- parse <$> readFile "day23/input.txt"
    print . part1 $ input
    print part2