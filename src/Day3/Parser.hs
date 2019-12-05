{-# LANGUAGE GADTs #-}
module Day3.Parser ( input )
where

import qualified Parsing                       as P
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text                     as T
import           Day3.Types
import           Data.Functor                   ( (<$) )

direction :: P.Parser Direction
direction = foldr1 (<|>) (f <$> [U ..])
  where f d = d <$ string (T.pack $ show d)

vector :: P.Parser Vector
vector = Vector <$> direction <*> P.int

wirePath :: P.Parser WirePath
wirePath = vector `sepBy1` P.comma

input :: P.Parser [WirePath]
input = many wirePath
