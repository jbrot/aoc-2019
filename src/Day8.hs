{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Day8 (day8) where

import Data.Char (isDigit)
import Data.List (minimumBy)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Options.Applicative

import CLI

type Width = Int
type Height = Int
type Image = (Width, Height, Vector Int)

countLayers :: Image -> Int
countLayers (w,h,c) = (`div` (w * h)) . length $ c

getLayer :: Int -> Image -> Vector Int
getLayer i (w,h,c) = V.slice (w * h * i) (w * h) c

combineLayers :: Vector Int -> Vector Int -> Vector Int
combineLayers = V.zipWith merge
  where merge 2 x = x
        merge x _ = x

render :: Image -> Image
render i@(w,h,_) = ((,,) w h) . foldr1 combineLayers . fmap (flip getLayer i) $ lyrs
    where lyrs = [0..(countLayers i) - 1]

renderPixel :: Int -> String
renderPixel 0 = "\x1b[40m "
renderPixel 1 = "\x1b[47m "
renderPixel _ = "\x1b[41m "

display :: Image -> IO ()
display (w,_,c) = putStr . mconcat . fmap ((<> "\x1b[0m\n") . V.foldr (\a b -> renderPixel a <> b) "") . divide $ c
    where divide v | null v = []
                   | otherwise = let (h,t) = V.splitAt w v in h:(divide t)

getInput :: Width -> Height -> IO Image
getInput w h = fmap (((,,) w h) . V.fromList . fmap (read . pure) . filter isDigit) getContents

count :: Eq a => a -> Vector a -> Int
count v = V.foldr (\v' c -> if v == v' then 1 + c else c) 0

star1 :: IO ()
star1 = do
    img <- getInput 25 6
    let lyrs = [0..(countLayers img) - 1]
        minL = minimumBy (\l1 l2 -> compare (count 0 (getLayer l1 img)) (count 0 (getLayer l2 img))) lyrs
    print $ (count 1 (getLayer minL img)) * (count 2 (getLayer minL img))

star2 :: IO ()
star2 = (display . render) =<< getInput 25 6

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day8 = AnyDay (Day parseStar runStar "day8" "Day 8 of AoC 2019")
