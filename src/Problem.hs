{-# LANGUAGE ExistentialQuantification, RecordWildCards #-}
module Problem where

import           Text.Parsec                    ( ParseError )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

data Problem = forall a b c. (Show b, Show c) =>
  Problem { parse :: Text -> Either ParseError a
          , solve :: a -> (b, c) }

solveProblem :: Problem -> Text -> Either ParseError (Text, Text)
solveProblem Problem {..} input = do
  (sol1, sol2) <- solve <$> parse input
  return (T.pack (show sol1), T.pack (show sol2))
