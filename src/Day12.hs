{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, LambdaCase #-}
module Day12 (day12) where

import qualified Control.Applicative as A
import Data.Either (either)
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as H
import Data.Vector (Vector)
import qualified Data.Vector as V
import Options.Applicative (Parser, flag, short, long, help)
import Text.Parsec

import CLI

data Coord = Coord { disp :: Int
                   ,  vel :: Int
                   } deriving (Eq, Ord, Show)

data Moon = Moon {  x :: Coord
                 ,  y :: Coord
                 ,  z :: Coord
                 } deriving (Eq, Ord, Show)

type Moons  = Vector Moon
type Coords = Vector Coord

instance Hashable Coords where
    hashWithSalt s l = V.foldr (\(Coord d v) tot -> (tot * a^2 + v * a + d) `mod` p) s l
      where p = 2147483647
            a =  406491133

stepVelPair :: Coord -> Coord -> (Coord, Coord)
stepVelPair (Coord d1 v1) (Coord d2 v2) = (Coord d1 (v1 + delta), Coord d2 (v2 - delta))
  where delta = signum (d2 - d1)

stepDisp :: Coord -> Coord
stepDisp (Coord d v) = Coord (d + v) v

pot :: Moon -> Int
pot m = (abs . disp . x $ m) + (abs . disp . y $ m) + (abs . disp . z $ m)

kin :: Moon -> Int
kin m = (abs . vel . x $ m) + (abs . vel . y $ m) + (abs . vel . z $ m)

tot :: Moon -> Int
tot m = pot m * kin m

intP :: Stream s m Char => ParsecT s u m Int
intP = do
    sgn <- A.optional (char '-')
    val <- read <$> many digit
    pure $ maybe val (const (-val)) sgn

coordP :: Stream s m Char => ParsecT s u m Coord
coordP = Coord <$> intP <*> pure 0

moonP :: Stream s m Char => ParsecT s u m Moon
moonP = Moon <$> (string "<x="  >> coordP)
             <*> (string ", y=" >> coordP) 
             <*> (string ", z=" >> coordP) 
             <* (char '>' >> endOfLine)

moonsP :: Stream s m Char => ParsecT s u m Moons
moonsP = V.fromList <$> many moonP

stepVelocity :: Coords -> Coords 
stepVelocity v = foldr stepPair v pairs
    where pairs = [(x,y) | x <- [0..(length v - 2)], y <- [(x + 1)..(length v - 1)] ]
          stepPair (i1,i2) ms = let (m1,m2) = stepVelPair (ms V.! i1) (ms V.! i2) in ms V.// [(i1,m1),(i2,m2)]

stepCoords  :: Coords -> Coords
stepCoords = V.map stepDisp . stepVelocity

stepMoons :: Moons -> Moons
stepMoons m = V.zipWith3 (Moon) x' y' z'
    where x' = stepCoords (V.map x m)
          y' = stepCoords (V.map y m)
          z' = stepCoords (V.map z m)

applyN :: Int -> (a -> a) -> a -> a
applyN n = foldr (.) id . replicate n

star1 :: IO ()
star1 = getContents >>= (either print (print . V.sum . V.map tot . applyN 1000 stepMoons) . parse moonsP "stdin")

loopUntilRepeat :: HashSet Coords -> Coords -> Int
loopUntilRepeat s c = if H.member c' s then 0
                                       else 1 + loopUntilRepeat (H.insert c' s) c'
    where c' = stepCoords c

firstRepeat :: Moons -> Int
firstRepeat m = lcm xr (lcm yr zr)
  where xr = loopUntilRepeat H.empty (V.map x m)
        yr = loopUntilRepeat H.empty (V.map y m)
        zr = loopUntilRepeat H.empty (V.map z m)

star2 :: IO ()
star2 = getContents >>= (either print (print . firstRepeat) . parse moonsP "stdin")

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day12 = AnyDay (Day parseStar runStar "day12" "Day 12 of AoC 2019")
