{-# LANGUAGE TemplateHaskell #-}
module Properties where

import           Parser                               (Expr (..),
                                                       parseLispValue,
                                                       parseQuote)

import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.QuickCheck
import           Text.ParserCombinators.Parsec

-- some tests would go here hopefully

tests = $testGroupGenerator
