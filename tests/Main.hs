module Main where

import           Properties
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Runners.Console       (defaultMain)
import           Test.QuickCheck

main = defaultMain [ Properties.tests ]
