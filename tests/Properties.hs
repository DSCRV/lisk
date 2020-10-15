{-# LANGUAGE TemplateHaskell #-}
module Properties (
                  runTests
                  ) where

import           Data.Maybe      (fromJust)
import           Error.Base      (unwrap)
import           Evaluator       (eval)
import           Operators       (primitives)
import           Parser          (Expr (..), parseLispValue, parseQuote)
import           Test.QuickCheck


prop_commutativeAdd :: [Integer] -> Property
prop_commutativeAdd xs =
    not (null xs) ==> rhs == lhs
        where rhs = (unwrap . addition) exprs
              lhs = (unwrap . addition . reverse) exprs
              exprs = map IntLiteral xs
              addition = fromJust $ lookup "+" primitives

prop_commutativeMul :: [Integer] -> Property
prop_commutativeMul xs =
    not (null xs) ==> rhs == lhs
        where rhs = (unwrap . multiplication) exprs
              lhs = (unwrap . multiplication . reverse) exprs
              exprs = map IntLiteral xs
              multiplication = fromJust $ lookup "*" primitives

return []
runTests = $quickCheckAll
