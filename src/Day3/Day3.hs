{-# LANGUAGE RecordWildCards, OverloadedLists #-}
module Day3.Day3 where

import           Problem
import           Control.Arrow                  ( (&&&) )
import qualified Data.Set                      as Set
import           Control.Monad.State
import           Day3.Types
import qualified Day3.Parser                   as Parser
import           Safe.Foldable
import qualified Data.Map                      as Map

manhattanDist :: Point -> Point -> Int
manhattanDist (Point x1 y1) (Point x2 y2) = abs (x2 - x1) + abs (y2 - y1)

movePoint :: Vector -> Point -> Point
movePoint Vector {..} (Point sx sy) = case dir of
  U -> Point sx (sy + dist)
  R -> Point (sx + dist) sy
  D -> Point sx (sy - dist)
  L -> Point (sx - dist) sy

createWireSegment :: Int -> Vector -> Point -> Wire
createWireSegment d Vector {..} (Point sx sy) = Map.fromList
  (zip points [d ..])
 where
  points = case dir of
    U -> Point sx <$> ([sy .. sy + dist] :: [Int])
    R -> flip Point sy <$> [sx .. sx + dist]
    D -> Point sx <$> [sy - dist .. sy]
    L -> flip Point sy <$> [sx - dist .. sx]

createWire :: Point -> WirePath -> Wire
createWire startPoint =
  mconcat . flip evalState (0, startPoint) . traverse segment
 where
  segment :: Vector -> State (Int, Point) Wire
  segment vec = do
    (d, point) <- get
    let (wireSegment, endPoint) =
          (createWireSegment d vec &&& movePoint vec) point
    put (d + dist vec, endPoint)
    return wireSegment

intersections :: Point -> [Wire] -> Map.Map Point Int
intersections start wires =
  Map.delete start $ foldr1 (Map.intersectionWith (+)) wires

closestIntersection :: Point -> [Wire] -> Maybe Int
closestIntersection start wires =
  let intersectionPoints = Map.keysSet $ intersections start wires
  in  minimumMay (manhattanDist start `Set.map` intersectionPoints)

solve1 :: [WirePath] -> Maybe Int
solve1 paths =
  let start = Point 0 0
      wires = createWire start <$> paths
  in  closestIntersection start wires

solve2 :: [WirePath] -> Maybe Int
solve2 paths =
  let start              = Point 0 0
      wires              = createWire start <$> paths
      intersectionPoints = intersections start wires
  in  minimumMay $ Map.elems intersectionPoints

problem :: Problem
problem = Problem { parser = Parser.input, solve = solve1 &&& solve2 }
