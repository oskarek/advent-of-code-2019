module Utils where

-- | Get all adjacent elements paired up
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 xs)

-- | Returns whether the list is in non-descending order
inOrder :: Ord a => [a] -> Bool
inOrder = all (uncurry (<=)) . pairs

-- | Returns whether the value meets all given predicates
meetsAll :: Foldable f => f (a -> Bool) -> a -> Bool
meetsAll preds x = all ($ x) preds