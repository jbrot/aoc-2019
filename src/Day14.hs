{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, LambdaCase #-}
module Day14 (day14) where

import Data.Either (either)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Options.Applicative (Parser, flag, short, long, help)
import Text.Parsec

import CLI

type Ingredient = String
type Quant = Integer
type Component = (Ingredient, Quant)
type Components = Map Ingredient Quant

type RecipeBook = Map Ingredient (Quant, Components)

type Recipe = (Ingredient, (Quant, Components))

intP :: Stream s m Char => ParsecT s u m Quant
intP = read <$> many digit

componentP :: Stream s m Char => ParsecT s u m Component
componentP = flip (,) <$> intP <*> (space >> many letter)

recipeP :: Stream s m Char => ParsecT s u m Recipe
recipeP = do
    inputs <- M.fromList <$> sepBy1 componentP (string ", ")
    string " => "
    (ing, quant) <- componentP
    endOfLine
    pure (ing, (quant, inputs))

recipeBookP :: Stream s m Char => ParsecT s u m RecipeBook
recipeBookP = M.fromList <$> many recipeP

divUp :: Quant -> Quant -> Quant
divUp n d = (n `div` d) + extra
    where extra = if n `mod` d == 0 then 0
                                    else 1

expandComponent :: RecipeBook -> Component -> Components -> (Components,Components)
expandComponent rs (ing,quant) extra = if M.notMember ing rs -- We're a base component, don't do anything.
                                          then (M.singleton ing quant, extra)
                                          else (outNeeded, outExtra)
    where (rquant, comps) = rs M.! ing
          extraI = M.findWithDefault 0 ing extra
          rcount = max 0 $ (quant - extraI) `divUp` rquant
          outNeeded = if rcount > 0 then M.map (*rcount) comps else M.empty
          leftOver = extraI + (rquant * rcount) - quant
          outExtra = M.insert ing leftOver extra

resolveStep :: RecipeBook -> (Components, Components) -> (Components, Components)
resolveStep rs (needed, extra) = M.foldrWithKey expand (M.empty,extra) needed
    where expand ing quant (out,extra) = let (out',extra') = expandComponent rs (ing,quant) extra in (M.unionWith (+) out out', extra')

resolve :: RecipeBook -> Components -> Quant
resolve rs c = go (c, M.empty)
    where go (needed,extra) = case M.keys needed of
                                ["ORE"] -> needed M.! "ORE"
                                _ -> go (resolveStep rs (needed,extra))

star1 :: IO ()
star1 = getContents >>= (either print (print . flip resolve (M.fromList [("FUEL", 1)])) . parse recipeBookP "stdin")

test :: Quant -> Quant -> RecipeBook -> Bool
test max qty rs = resolve rs (M.singleton "FUEL" qty) <= max

-- Find how much FUEL can be made with the specified amount of ORE
maximize :: Quant -> RecipeBook -> Quant
maximize mx rs = go 1 1
    where go qty step | test mx (qty + 2 * step) rs = go (qty + 2 * step) (2 * step)
                      | test mx (qty + step) rs = go (qty + step) (max 1 (step `div` 2))
                      | step == 1 = qty
                      | otherwise = go qty (max 1 (step `div` 2))

star2 :: IO ()
star2 = getContents >>= (either print (print . maximize (1 * 10^12)) . parse recipeBookP "stdin")

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day14 = AnyDay (Day parseStar runStar "day14" "Day 14 of AoC 2019")
