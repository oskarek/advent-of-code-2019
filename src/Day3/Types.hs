module Day3.Types where

import qualified Data.Map                      as Map

data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord, Show)
type Wire = Map.Map Point Int

data Direction = U | R | D | L deriving (Eq, Show, Enum)
data Vector = Vector { dir :: Direction, dist :: Int }
type WirePath = [Vector]
