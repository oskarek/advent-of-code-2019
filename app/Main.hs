{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Map                       ( Map
                                                , (!?)
                                                )
import qualified Data.Map                      as M
import           Options.Applicative     hiding ( ParseError )
import           Problem
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import qualified Day1.Day1                     as Day1

problems :: Map Int Problem
problems = M.fromList
    [ (1, Day1.problem)
    ]

printSolutions :: (Text, Text) -> IO ()
printSolutions (s1, s2) =
    TIO.putStrLn
        $  "Solution to part 1:\n"
        <> s1
        <> "\n\n"
        <> "Solution to part 2:\n"
        <> s2

main :: IO ()
main = do
    day <- execParser opts
    case problems !? day of
        Nothing -> putStrLn $ "Day " ++ show day ++ " problem not solved yet!"
        Just problem -> do
            input <- TIO.readFile ("input/day" ++ show day ++ ".txt")
            let sol = solveProblem problem input
            either print printSolutions sol
  where
    opts = info
        (day <**> helper)
        (fullDesc <> progDesc "Solve the puzzles for DAY" <> header
            "Advent of Code 2018 solutions"
        )
    day =
        argument auto (help "The number of the day to solve" <> metavar "DAY")
