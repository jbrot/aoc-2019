{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Day6 (day6) where

import Data.Either (either)
import Data.Map (Map)
import qualified Data.Map as M
import Options.Applicative (Parser, flag, short, long, help)
import Text.Parsec

import CLI

type Body = String
data Orbit = Orbit { com :: Body
                   , body :: Body
                   } deriving Show

orbP :: Stream s m Char => ParsecT s u m Orbit
orbP = Orbit <$> manyTill alphaNum (char ')') <*> manyTill alphaNum endOfLine

orbitMap :: [Orbit]  -> Map Body Body
orbitMap = M.fromList . fmap (\o -> (body o, com o))

countOrbits :: Map Body Body -> Body -> Int
countOrbits m = maybe 0 ((+ 1) . countOrbits m)  . (flip M.lookup m) 

countAllOrbits ::  Map Body Body -> Int
countAllOrbits m = M.foldrWithKey (\b _ c -> c + countOrbits m b) 0 m

distance :: Body -> Body -> Map Body Body -> Int
distance b1 b2 = snd . tag b2 (-1) . fst . tag b1 (-1) . fmap (\x -> (x,0))
    where tag :: Body -> Int -> Map Body (Body, Int) -> (Map Body (Body, Int), Int)
          tag b c m = maybe (m,-1) (\(b',c') -> if c' == 0
                                                   then tag b' (c + 1) (M.insert b (b',c) m)
                                                   else (m, c  + c')) (M.lookup b m)

star1 :: IO ()
star1 = getContents >>= (either print (print . countAllOrbits . orbitMap) . parse (many orbP) "stdin")

star2 :: IO ()
star2 = getContents >>= (either print (print . distance "YOU" "SAN" . orbitMap) . parse (many orbP) "stdin")

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day6 = AnyDay (Day parseStar runStar "day6" "Day 6 of AoC 2019")
