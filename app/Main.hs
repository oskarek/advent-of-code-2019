module Main where

import           Data.Map                       ( Map, (!?) )
import qualified Data.Map                      as M
import           Options.Applicative
import           Problem
import qualified Data.Text.IO                  as TIO

import qualified Day1.Day1                     as Day1
import qualified Day2.Day2                     as Day2
import qualified Day3.Day3                     as Day3

problems :: Map Int Problem
problems = M.fromList
  [ (1, Day1.problem)
  , (2, Day2.problem)
  , (3, Day3.problem) ]

main :: IO ()
main = do
  day <- execParser opts
  case problems !? day of
    Nothing      -> putStrLn $ "Day " ++ show day ++ " problem not solved yet!"
    Just problem -> do
      input <- TIO.readFile ("input/day" ++ show day ++ ".txt")
      putStrLn $ printSolution problem input
 where
  opts = info
    (dayArg <**> helper)
    (fullDesc <> progDesc "Solve the puzzles for DAY" <> header
      "Advent of Code 2018 solutions"
    )
  dayArg = argument auto (help "The number of the day to solve" <> metavar "DAY")
