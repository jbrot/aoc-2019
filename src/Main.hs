module Main where

import Control.Monad
import Options.Applicative

import CLI
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12

days :: [AnyDay]
days = [ day1,  day2,  day3,  day4,  day5,  day6,  day7,  day8,  day9,  day10
       , day11, day12
       ]

parser :: ParserInfo (IO ())
parser = info (helper <*> p) (progDesc "This program is designed to solve the 2019 Advent of Code")
    where p = hsubparser . mconcat . fmap anyDayToParser $ days

main :: IO ()
main = join . execParser $ parser
