module Parsing where

import qualified Text.Parsec                   as P
import qualified Text.Parsec.Text              as PT
import           Data.Functor                   ( (<$) )
import           Data.Text                      ( Text )

modif :: PT.Parser (Int -> Int)
modif = negate <$ P.char '-' P.<|> pure id

int :: PT.Parser Int
int = modif <*> (read <$> P.many1 P.digit <* P.spaces)

parse :: PT.Parser a -> Text -> Either P.ParseError a
parse p = P.parse p ""
