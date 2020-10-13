{-# LANGUAGE TemplateHaskell #-}
module Properties where

import           Parser          (Expr (..), parseLispValue, parseQuote)

import           Test.QuickCheck

-- some tests would go here hopefully

-- a filler test to test the test suite :^)
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort left ++ [x] ++ qsort right
    where left = filter (<= x) xs
          right = filter (> x) xs

checkList :: (Ord a) => [a] -> Bool
checkList = ordered . qsort
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

return []
tests = $quickCheckAll
