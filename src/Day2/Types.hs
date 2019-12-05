module Day2.Types where

import qualified Data.Map                      as M

data BinOpType = Add | Mult deriving (Eq, Show)
data Instr =
    Halt
  | BinOpInstr { opType :: BinOpType, idx1 :: Int, idx2 :: Int, dest :: Int }
  deriving (Eq, Show)
type Board = M.Map Int Int

mkBoard :: [Int] -> Board
mkBoard = M.fromAscList . zip [0 ..]