{-# LANGUAGE ScopedTypeVariables #-}
module Day5 (day5) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List.Index (imap)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Options.Applicative

import Common
import CLI

type Program = Vector Int
type Address = Int
type Pointer = Address
type ProgT = StateT (Program, Pointer)

data Opcode = Add | Multiply | Halt | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals
data ParamMode = Position | Immediate

type Input m = m Int
type Output m = Int -> m ()

toParamModes :: Int -> [ParamMode]
toParamModes = foldlDigits parse (repeat Position)
  where parse modes 0 = Position:modes
        parse modes 1 = Immediate:modes
        parse _ _ = undefined

toOpcode :: Int -> Opcode
toOpcode 1 = Add
toOpcode 2 = Multiply
toOpcode 3 = Input
toOpcode 4 = Output
toOpcode 5 = JumpIfTrue
toOpcode 6 = JumpIfFalse
toOpcode 7 = LessThan
toOpcode 8 = Equals
toOpcode 99 = Halt
toOpcode _ = undefined

parseOpcode :: Int -> (Opcode, [ParamMode])
parseOpcode n = (toOpcode (n `mod` 100), toParamModes (n `div` 100))

getParameter :: Monad m => Int -> ParamMode -> ProgT m (Int, Address)
getParameter o m = do
    (p, ptr) <- get
    let val = p V.! (ptr + o + 1)
    case m of
      Position -> pure (p V.! val, val)
      Immediate -> pure (val, undefined)

getOpcode :: Monad m => ProgT m (Opcode, [ProgT m (Int, Address)])
getOpcode = fmap (fmap (imap getParameter) . parseOpcode . uncurry (V.!)) get 

-- Step the program; returns True if halt, False if we should still run
stepProgram :: Monad m => Input m -> Output m -> ProgT m Bool
stepProgram inp outp = do
    (p, ptr) <- get
    (op, params) <- getOpcode
    (p1,p2,p3) <- case params of
                    p1:p2:p3:_ -> pure (p1,p2,p3)
                    _ -> undefined
    case op of
      Add -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          put (p V.// [(c, a + b)], ptr + 4)
          pure False
      Multiply -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          put (p V.// [(c, a * b)], ptr + 4)
          pure False
      Input -> do
          (_,a) <- p1
          v <- lift inp
          put (p V.// [(a, v)], ptr + 2)
          pure False
      Output -> do
          (a,_) <- p1
          lift $ outp a
          put (p, ptr + 2)
          pure False
      JumpIfTrue -> do
          (a,_) <- p1
          (b,_) <- p2
          if a /= 0 then put (p, b)
                    else put (p, ptr + 3)
          pure False
      JumpIfFalse -> do
          (a,_) <- p1
          (b,_) <- p2
          if a == 0 then put (p, b)
                    else put (p, ptr + 3)
          pure False
      LessThan -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          if a < b then put (p V.// [(c, 1)], ptr + 4)
                   else put (p V.// [(c, 0)], ptr + 4)
          pure False
      Equals -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          if a == b then put (p V.// [(c, 1)], ptr + 4)
                    else put (p V.// [(c, 0)], ptr + 4)
          pure False
      Halt -> pure True

runProgram :: forall m. Monad m => Input m -> Output m -> Program -> m Program
runProgram i o = fmap fst . execStateT go . (flip (,) 0)
    where go :: ProgT m ()
          go = stepProgram i o >>= flip unless go

-- type Noun = Int
-- type Verb = Int
-- execProg :: Noun -> Verb -> Program -> Int
-- execProg n v p = (runProgram p') V.! 0
--   where p' = p V.// [(1,n), (2,v)]

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

parseInput :: IO Program
parseInput = fmap (V.fromList . fmap read . split ',') getLine

star1 :: IO ()
star1 = parseInput >>= runProgram i o >>= print
  where i = pure 1
        o = print

star2 :: IO ()
star2 = parseInput >>= runProgram i o >>= print
  where i = pure 5
        o = print

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day5 = AnyDay (Day parseStar runStar "day5" "Day 5 of AoC 2019")
