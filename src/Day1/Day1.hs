module Day1.Day1 ( problem ) where

import           Problem
import qualified Data.Text                     as T
import qualified Parsing                       as P
import           Control.Arrow                  ( (&&&) )

f :: Int -> Int
f x = x `div` 3 - 2

g :: Int -> Int
g x = if y > 0 then y + g y else 0 where y = f x

problem :: Problem
problem = Problem { parse = mapM (P.parse P.int)
                  , solve = sumWith f &&& sumWith g
                  }
  where sumWith f = sum . map f
