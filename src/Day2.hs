module Day2 (day2) where

import Control.Monad
import Control.Monad.Trans.State
import Data.Vector (Vector)
import qualified Data.Vector as V
import Options.Applicative

import CLI

type Program = Vector Int
type Pointer = Int
type ProgM = State (Program, Pointer)

-- Step the program; returns True if halt, False if we should still run
stepProgram :: ProgM Bool
stepProgram = do
    (p, ptr) <- get
    case (p V.! ptr) of
        1 -> let a = p V.! (p V.! (ptr + 1))
                 b = p V.! (p V.! (ptr + 2))
                 upd = [(p V.! (ptr + 3), a + b)]
              in put (p V.// upd, ptr + 4) >> pure False
        2 -> let a = p V.! (p V.! (ptr + 1))
                 b = p V.! (p V.! (ptr + 2))
                 upd = [(p V.! (ptr + 3), a * b)]
              in put (p V.// upd, ptr + 4) >> pure False
        99 -> pure True
        _  -> undefined

runProgram :: Program -> Program
runProgram = fst . execState go . (flip (,) 0)
    where go :: ProgM ()
          go = stepProgram >>= flip unless go

type Noun = Int
type Verb = Int
execProg :: Noun -> Verb -> Program -> Int
execProg n v p = (runProgram p') V.! 0
  where p' = p V.// [(1,n), (2,v)]

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

parseInput :: IO Program
parseInput = fmap (V.fromList . fmap read . split ',') getLine

star1 :: IO ()
star1 = parseInput >>= (print . execProg 12 2)

star2 :: IO ()
star2 = parseInput >>= (\p -> print =<< foldr (step p) undefined possible)
  where possible = [(x,y) | x <- [0..99], y <- [0..99]]
        step p (n,v) o = if execProg n v p == 19690720 then pure (100 * n + v) else o

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day2 = AnyDay (Day parseStar runStar "day2" "Day 2 of AoC 2019")
