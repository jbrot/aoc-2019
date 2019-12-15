{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Day10 (day10) where

import Data.Either (either)
import Data.Foldable (foldlM)
import Data.List (maximumBy, sortBy)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Options.Applicative (Parser, flag, short, long, help)
import Text.Parsec

import CLI

type Row = Int
type Col = Int
type AsteroidField = Vector (Vector Bool)

entryP :: Stream s m Char => ParsecT s u m Bool
entryP = (char '.' >> pure False) <|>  (char '#' >> pure True)

rowP :: Stream s m Char => ParsecT s u m (Vector Bool)
rowP = fmap (V.fromList) $ manyTill entryP endOfLine

fieldP :: Stream s m Char => ParsecT s u m AsteroidField
fieldP = fmap (V.fromList) (many rowP)

asteroid :: Row -> Col -> AsteroidField -> Bool
asteroid r c f = (f V.! r) V.! c

rayCast :: Row -> Col -> Int -> Int -> AsteroidField -> Maybe (Row, Col)
rayCast r c dr dc f | r' < 0 = Nothing
                    | r' >= rows = Nothing
                    | c' < 0 = Nothing
                    | c' >= cols = Nothing
                    | asteroid r' c' f = Just (r',  c')
                    | otherwise = rayCast r' c' dr dc f
    where rows = length f
          cols = length (f V.! 0)
          r' = r + dr
          c' = c + dc

--     -
--    4|1
--  - --- +
--    3|2
--     +
quadrant :: (Int, Int) -> Int
quadrant (r,c)
  | r <  0 && c >= 0 = 1
  | r >= 0 && c >= 0 = 2
  | r >  0 && c <  0 = 3
  | r <= 0 && c <  0 = 4
  | otherwise = undefined -- Can't be reached, but makes GHC happy

cvrt :: (Integral a, Num b) => a ->  b
cvrt = fromInteger . toInteger

clockwiseSort :: (Int,Int) -> (Int,Int) -> Ordering
clockwiseSort p1@(r1,c1) p2@(r2,c2)
  | quadrant p1 /= quadrant p2 = compare (quadrant p1) (quadrant p2)
  | otherwise = compare (cvrt r1 / cvrt c1) (cvrt r2 / cvrt c2)

possibleOffsets :: Row -> Col -> AsteroidField -> [(Int,Int)]
possibleOffsets r c f = sortBy clockwiseSort offsets 
  where rows = length f
        cols = length (f  V.! 0)
        offsets = filter (\(dr,dc) -> gcd dr dc == 1) [ (dr,dc) | dr <- [(-r)..(rows - r - 1)], dc <- [(-c)..(cols - c - 1)] ]

countVisible :: Row -> Col -> AsteroidField -> Int
countVisible r c f = if not (asteroid r c f) then 0 else tot
  where tot = foldr (\(dr,dc) ct -> maybe ct (const (ct + 1)) (rayCast r c dr dc f)) 0 (possibleOffsets r c f)

star1 :: IO ()
star1 = getContents >>= (either print (print . maxCount) . parse fieldP "stdin")
    where maxCount f = let inds = [ (r,c) | r <- [0..(length f - 1)], c <- [0..(length (f V.! 0) - 1)]]
                        in maximum (fmap (\(r,c) -> countVisible r c f) inds)

bestVisible :: AsteroidField -> (Row,Col)
bestVisible f = maximumBy (\(r1,c1) (r2,c2) -> compare (countVisible r1 c1 f) (countVisible r2 c2 f)) inds
    where inds = [ (r,c) | r <- [0..(length f - 1)], c <- [0..(length (f V.! 0) - 1)]]


vaporize :: Row -> Col -> AsteroidField ->  AsteroidField
vaporize r c f = f V.// [(r, (f V.! r) V.// [(c, False)])]

vaporizeAll :: Monad m => (Int -> (Row,Col) -> m ()) -> Row -> Col -> AsteroidField -> m ()
vaporizeAll log r c field = go (field,1)
  where offsets = possibleOffsets r c field
        sweep st = foldlM (\st (dr,dc) -> maybe (pure st) (hit st) (rayCast r c dr dc (fst st))) st offsets
        hit (f,ct) (r',c') = log ct (r',c') >> pure (vaporize  r' c' f, ct + 1)
        go st = do
            st' <- sweep st
            if snd st == snd st' then pure ()
                                 else go st'

star2 :: IO ()
star2 = getContents >>= (either print go . parse fieldP "stdin")
    where go f = let (r,c) = bestVisible f in vaporizeAll log r c f
          log 200  (r,c) = print (100 * c + r)
          log _ _ = pure  ()

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 


day10 = AnyDay (Day parseStar runStar "day10" "Day 10 of AoC 2019")
