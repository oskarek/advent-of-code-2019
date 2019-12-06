{-# LANGUAGE OverloadedStrings #-}
module Day4.Day4 where

import           Problem
import qualified Parsing                       as P
import           Control.Arrow                  ( (&&&) )
import qualified Data.List.NonEmpty            as NE

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 xs)

inOrder :: Ord a => [a] -> Bool
inOrder = all (uncurry (<=)) . pairs

meetsAll :: [a -> Bool] -> a -> Bool
meetsAll preds x = all ($ x) preds

digitStrings :: (Int, Int) -> [[NE.NonEmpty Char]]
digitStrings (start, end) = NE.group . show <$> [start .. end]

solve1 :: (Int, Int) -> Int
solve1 = length . filter (meetsAll predicates) . digitStrings
  where predicates = [any ((>= 2) . length), inOrder]

solve2 :: (Int, Int) -> Int
solve2 = length . filter (meetsAll predicates) . digitStrings
  where predicates = [any ((== 2) . length), inOrder]

problem :: Problem
problem = Problem { parser = (,) <$> P.int <*> (P.symbol "-" *> P.int)
                  , solve  = solve1 &&& solve2
                  }
