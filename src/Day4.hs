module Day4 (day4) where

import Options.Applicative (Parser, flag, short, long, help)

import Common
import CLI

hasAdjacentDigits :: Int -> Bool
hasAdjacentDigits = snd . foldlDigits (\(n, b) n' -> (n', b || n == n')) (-1, False)

doubleDigits :: Int -> Bool
doubleDigits = snd . flip step (-1) . foldlDigits step ((-1,-2,-3), False)
  where step ((n1, n2, n3), b) n4 = ((n2, n3, n4), b || (n1 /= n2 && n2 == n3 && n3 /= n4))

nonDecreasing :: Int -> Bool
nonDecreasing = snd . foldlDigits (\(n, b) n' -> (n', b && n <= n')) (-1, True)

filteredRange :: (Int, Int) -> [Int]
filteredRange (l,h) = filter (\x -> hasAdjacentDigits x && nonDecreasing x) [l..h]

filteredRange2 :: (Int, Int) -> [Int]
filteredRange2 (l,h) = filter (\x -> doubleDigits x && nonDecreasing x) [l..h]

star1 :: IO ()
star1 = (print . length . filteredRange) =<< readLn

star2 :: IO ()
star2 = (print . length . filteredRange2) =<< readLn

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day4 = AnyDay (Day parseStar runStar "day4" "Day 4 of AoC 2019")
