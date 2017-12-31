{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}

import Data.Char (isAlpha)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromJust)
import Data.Monoid (First(..), Last(..), getFirst, getLast, (<>))
import qualified Data.Vector as V
import Control.Lens
import Control.Applicative (many, empty)
import Control.Monad (when, guard, void)
import Control.Monad.Prompt (Prompt, runPromptM, prompt)
import Control.Monad.State (State, StateT, get, put, modify, evalState, execStateT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Writer (Writer, WriterT, tell, runWriterT, execWriter)

type Param = Either Char Int

data Op
    = Snd Param
    | Set Char Param
    | Bin (Int -> Int -> Int) Char Param
    | Rcv Char
    | Jgz Param Param

parse :: String -> V.Vector Op
parse = V.fromList . mapMaybe (go . words) . lines
    where go ["snd", x]     = Just $ Snd (f x)
          go ["set", x, y]  = Just $ Set (head x) (f y)
          go ["add", x, y]  = Just $ Bin (+) (head x) (f y)
          go ["mul", x, y]  = Just $ Bin (*) (head x) (f y)
          go ["mod", x, y]  = Just $ Bin mod (head x) (f y)
          go ["rcv", x]     = Just $ Rcv (head x)
          go ["jgz", x, y]  = Just $ Jgz (f x) (f y)
          go _              = Nothing
          f [c] | isAlpha c = Left c
          f str             = Right $ read str

data Program = Program
    { _pc     :: Int
    , _regs   :: M.Map Char Int
    , _code   :: V.Vector Op
    }
makeLenses ''Program

data Command
    = CSnd Int
    | CRcv Int

type Machine = Prompt Command Int
type Duet = MaybeT (StateT Program Machine)

reg :: Char -> Lens' Program Int
reg r = regs . at r . non 0

val :: Param -> Duet Int
val = either (use . reg) pure

send :: Int -> Duet Int
send = prompt . CSnd

receive :: Int -> Duet Int
receive = prompt . CRcv

perform :: Op -> Duet ()
perform (Set x y)   = (reg x .=) =<< val y
perform (Snd x)     = void $ val x >>= send
perform (Rcv x)     = use (reg x) >>= receive >>= assign (reg x)
perform (Bin f x y) = flip f <$> val y >>= modifying (reg x)
perform (Jgz x y)   = (> 0)  <$> val x >>= flip when ((pc +=) =<< pred <$> val y)

step :: Duet ()
step = do
    c <- use pc
    Just o <- preuse (code . ix c)
    perform o >> pc += 1

execDuet :: Duet a -> Program -> Machine Program
execDuet = execStateT . runMaybeT

type Part1 = StateT (Last Int) (Writer (First Int))

execPart1 :: Part1 a -> Int
execPart1 = fromJust . getFirst . execWriter . flip execStateT mempty

interpret1 :: Command -> Part1 Int
interpret1 (CSnd x) = modify (<> pure x) >> return x
interpret1 (CRcv x) = when (x /= 0) (tell . First . getLast =<< get) >> return x

part1 :: V.Vector Op -> Int
part1 = execPart1 . flip runPromptM interpret1 . execDuet (many step) . Program 0 M.empty

data Thread = Thread
    { _program :: Program 
    , _buffer :: [Int] 
    }
makeLenses ''Thread

type Part2 s = MaybeT (State s)

interpret2 :: Command -> WriterT [Int] (Part2 [Int]) Int
interpret2 (CSnd x) = tell [x] >> return x
interpret2 (CRcv _) = get >>= f
    where f []      = empty
          f (x:xs)  = put xs >> return x

stepThread :: Part2 Thread [Int]
stepThread = do
    machine   <- execDuet step <$> use program
    (ps, out) <- runWriterT . zoom buffer $ runPromptM machine interpret2
    program .= ps
    return out

type MultiThread = (Thread, Thread)

stepThreads :: Part2 MultiThread Int
stepThreads = do
    outA <- zoom _1 $ concat <$> many stepThread
    outB <- zoom _2 $ concat <$> many stepThread
    _1 . buffer <>= outB
    _2 . buffer <>= outA
    guard . not $ null outA && null outB
    return . length $ outB

part2 :: V.Vector Op -> Int
part2 c = sum . concat . evalState (runMaybeT $ many stepThreads) $ ms
    where ms = ( Thread (Program 0 (M.singleton 'p' 0) c) []
               , Thread (Program 0 (M.singleton 'p' 1) c) [] )

main :: IO ()
main = do
    input <- parse <$> readFile "day18/input.txt"
    print . part1 $ input
    print . part2 $ input