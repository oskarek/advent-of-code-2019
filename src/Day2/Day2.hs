{-# LANGUAGE RecordWildCards #-}
module Day2.Day2 ( problem ) where

import           Problem
import qualified Parsing                       as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Control.Arrow                  ( (&&&) )
import qualified Data.Map                      as M
import           Control.Monad                  ( foldM )
import           Data.List                      ( find )

data Op = Op (Int -> Int -> Int) | Halt
data Instr = Instr { op :: Op, idx1 :: Int, idx2 :: Int, dest :: Int }
type Board = M.Map Int Int

instrs :: [Int] -> Maybe [Instr]
instrs []             = Nothing
instrs (opInt : rest) = do
  op <- ops M.!? opInt
  case op of
    Halt -> Just []
    Op o ->
      let (params, rest') = splitAt 3 rest
      in  case params of
            [idx1, idx2, dest] -> (Instr op idx1 idx2 dest :) <$> instrs rest'
            _                  -> Nothing
  where ops = M.fromList [(1, Op (+)), (2, Op (*)), (99, Halt)]

applyOpcode :: Board -> Instr -> Maybe Board
applyOpcode board Instr {..} = do
  arg1 <- board M.!? idx1
  arg2 <- board M.!? idx2
  case op of
    Op op -> Just $ M.insert dest (arg1 `op` arg2) board
    Halt -> Just board

applyInput :: (Int, Int) -> Board -> Board
applyInput (noun, verb) = M.insert 2 verb . M.insert 1 noun

mkBoard :: [Int] -> Board
mkBoard = M.fromAscList . zip [0..]

instrsAndBoard :: (Int, Int) -> [Int] -> Maybe ([Instr], Board)
instrsAndBoard input boardList = do
  codes <- instrs boardList
  let board = applyInput input (mkBoard boardList)
  return (codes, board)

applyOpcodes :: Board -> [Instr] -> Maybe Board
applyOpcodes = foldM applyOpcode

runProgram :: [Int] -> (Int, Int) -> Maybe Int
runProgram boardList input = do
  (instrs, startBoard) <- instrsAndBoard input boardList
  newBoard             <- applyOpcodes startBoard instrs
  newBoard M.!? 0

solve1 :: [Int] -> Maybe Int
solve1 boardList = runProgram boardList (12, 2)

solve2 :: [Int] -> Maybe Int
solve2 boardList = do
  let inputs = [ (x, y) | x <- [0 .. 99], y <- [0 .. 99] ]
  (noun, verb) <- find ((== Just 19690720) . runProgram boardList) inputs
  return (100 * noun + verb)

problem :: Problem
problem = Problem { parser = P.int `sepBy` P.comma
                  , solve = solve1 &&& solve2
                  }
