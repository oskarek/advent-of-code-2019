{-# LANGUAGE OverloadedStrings #-}
module Day2.Parser
  ( input
  )
where

import qualified Parsing                       as P
import           Text.Megaparsec
import qualified Data.Map                      as M
import           Day2.Types
import qualified Data.Text                     as T

----- BINOP -----

-- Mapping between ints and BinOpType
binOpTypes :: M.Map Int BinOpType
binOpTypes = M.fromList [(1, Add), (2, Mult)]

binOpType :: P.Parser BinOpType
binOpType = foldr1 (<|>) (f <$> M.toList binOpTypes)
  where f (int, typ) = typ <$ P.symbol (T.pack $ show int)

binOpInstr :: P.Parser Instr
binOpInstr = BinOpInstr <$> binOpType <*> param <*> param <*> param
  where param = P.comma *> P.int

----- HALT -----

haltInstr :: P.Parser Instr
haltInstr = Halt <$ (P.symbol "99" *> skipMany (P.comma *> P.int))

----- BOARD -----

board :: P.Parser Board
board = mkBoard <$> (P.int `sepBy` P.comma)

----- INSTRS -----

instrs :: P.Parser [Instr]
instrs = (haltInstr <|> binOpInstr) `sepBy` P.comma

----- INPUT -----

input :: P.Parser (Board, [Instr])
input = (,) <$> lookAhead board <*> instrs
