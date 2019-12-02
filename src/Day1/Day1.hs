{-# LANGUAGE OverloadedStrings #-}
module Day1.Day1 ( problem ) where

import Problem
import qualified Data.Text as T

problem :: Problem
problem = Problem
  { parse = Right
  , solve = return ("", "") }