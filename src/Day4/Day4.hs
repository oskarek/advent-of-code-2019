{-# LANGUAGE OverloadedStrings #-}
module Day4.Day4 where

import           Problem
import qualified Parsing                       as P
import           Control.Arrow                  ( (&&&) )
import qualified Data.List.NonEmpty            as NE
import           Utils

digitStrings :: (Int, Int) -> [[NE.NonEmpty Char]]
digitStrings (start, end) = NE.group . show <$> [start .. end]

prob1Preds :: [[NE.NonEmpty Char] -> Bool]
prob1Preds = [any ((>= 2) . length), inOrder]

prob2Preds :: [[NE.NonEmpty Char] -> Bool]
prob2Preds = [any ((== 2) . length), inOrder]

solveWithPreds :: [[NE.NonEmpty Char] -> Bool] -> (Int, Int) -> Int
solveWithPreds preds = length . filter (meetsAll preds) . digitStrings

problem :: Problem
problem = Problem
  { parser = (,) <$> P.int <*> (P.symbol "-" *> P.int)
  , solve  = solveWithPreds prob1Preds &&& solveWithPreds prob2Preds
  }
