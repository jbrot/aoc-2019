{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Day3 (day3) where

import Data.Either (either)
import qualified Data.Set as S
import Options.Applicative (Parser, flag, short, long, help)
import Text.Parsec

import CLI

data Dir = U | D | L | R
    deriving (Show)
data Step = Step { dir   :: Dir
                 , dist  :: Int
                 } deriving (Show)
type Wire = [Step]

type Row = Int
type Col = Int
type Pos = (Row, Col)

coords :: Wire -> [Pos]
coords w = init $ go w (0,0) []
    where go :: Wire -> Pos -> [Pos] -> [Pos]
          go [] p ps = p:ps
          go ((Step _ 0):w) p ps = go w p ps
          go ((Step U n):w) (r,c) ps = go ((Step U (n - 1)):w) (r - 1, c) ((r,c):ps)
          go ((Step D n):w) (r,c) ps = go ((Step D (n - 1)):w) (r + 1, c) ((r,c):ps)
          go ((Step L n):w) (r,c) ps = go ((Step L (n - 1)):w) (r, c - 1) ((r,c):ps)
          go ((Step R n):w) (r,c) ps = go ((Step R (n - 1)):w) (r, c + 1) ((r,c):ps)

dirP :: Stream s m Char => ParsecT s u m Dir
dirP = flip fmap (oneOf "UDLR") $ \case
    'U' -> U
    'D' -> D
    'L' -> L
    'R' -> R
    _   -> undefined

intP :: Stream s m Char => ParsecT s u m Int
intP = fmap read (many1 digit)

stepP :: Stream s m Char => ParsecT s u m Step
stepP = Step <$> dirP <*> intP

wireP :: Stream s m Char => ParsecT s u m Wire
wireP = (stepP `sepBy` (char ',')) <* endOfLine

closestIntersection :: [Wire] -> Maybe Int
closestIntersection = S.lookupMin . S.map (\(r,c) -> abs r + abs c) . foldr1 S.intersection . def S.empty . fmap (S.fromList . coords)
  where def d [] = [d]
        def _ l  = l

star1 :: IO ()
star1 = getContents >>= (either print (print . closestIntersection) . parse (many wireP) "stdin")

star2 :: IO ()
star2 = undefined

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day3 = AnyDay (Day parseStar runStar "day3" "Day 3 of AoC 2019")
