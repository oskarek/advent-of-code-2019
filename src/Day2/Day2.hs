{-# LANGUAGE RecordWildCards #-}
module Day2.Day2 where

import           Problem
import           Control.Arrow                  ( (&&&) )
import qualified Data.Map                      as M
import           Control.Monad                  ( foldM, (<=<) )
import qualified Data.List                     as List
import           Day2.Types
import qualified Day2.Parser                   as Parser

binOpAction :: BinOpType -> Int -> Int -> Int
binOpAction typ = case typ of
  Add  -> (+)
  Mult -> (*)

applyInstr :: Board -> Instr -> Maybe Board
applyInstr board instr = case instr of
  Halt            -> Just board
  BinOpInstr {..} -> do
    val <- binOpAction opType <$> board M.!? idx1 <*> board M.!? idx2
    Just (M.insert dest val board)

applyInput :: (Int, Int) -> Board -> Board
applyInput (noun, verb) = M.insert 2 verb . M.insert 1 noun

runUntilEndState :: (Int, Int) -> Board -> [Instr] -> Maybe Board
runUntilEndState input board instrs = do
  let startBoard = applyInput input board
  foldM applyInstr startBoard instrs

extractOutput :: Board -> Maybe Int
extractOutput = (M.!? 0)

runProgram :: (Int, Int) -> Board -> [Instr] -> Maybe Int
runProgram input board = extractOutput <=< runUntilEndState input board

solve1 :: (Board, [Instr]) -> Maybe Int
solve1 = uncurry (runProgram (12, 2))

solve2 :: (Board, [Instr]) -> Maybe Int
solve2 (board, instrs) = do
  let inputs = [ (x, y) | x <- [0 .. 99], y <- [0 .. 99] ]
  (noun, verb) <- List.find ((== Just 19690720) . run) inputs
  return (100 * noun + verb)
  where run input =  runProgram input board instrs

problem :: Problem
problem = Problem { parser = Parser.input, solve = solve1 &&& solve2 }
