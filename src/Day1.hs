module Day1 (day1) where

import Data.Maybe
import Data.Monoid
import Options.Applicative
import System.IO
import Text.Read

import CLI

type Mass = Int
type Fuel = Int

getFuel :: Mass -> Fuel
getFuel = subtract 2 . (`div` 3)

getFuelRec :: Mass -> Fuel
getFuelRec m = let f = getFuel m in if f <= 0 then 0 else f + getFuelRec f

nextInput :: Read a => IO (Maybe a)
nextInput = do
    done <- isEOF
    if done
       then pure Nothing
       else fmap readMaybe getLine

sumInputsWithFunction :: (Read a, Monoid b) => (a -> b) -> IO b
sumInputsWithFunction f = foldr (\rd tot -> rd >>= maybe (pure mempty) (flip fmap tot . (<>) . f)) undefined (repeat nextInput)

star1 :: IO ()
star1 = print =<< (fmap getSum . sumInputsWithFunction $ Sum . getFuel)

star2 :: IO ()
star2 = print =<< (fmap getSum . sumInputsWithFunction $ Sum . getFuelRec)

runDay1 :: Bool -> IO ()
runDay1 False = star1
runDay1 True = star2

parseDay1 :: Parser Bool
parseDay1 = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day1 = AnyDay (Day parseDay1 runDay1 "day1" "Day 1 of AoC 2019")
