module Main where

import Control.Monad
import Options.Applicative

import CLI
import Day1

days :: [AnyDay]
days = [ day1 ]

parser :: ParserInfo (IO ())
parser = info (helper <*> p) (progDesc "This program is designed to solve the 2019 Advent of Code")
    where p = hsubparser . mconcat . fmap anyDayToParser $ days

main :: IO ()
main = join . execParser $ parser
