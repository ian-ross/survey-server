module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ literals ]

literals :: TestTree
literals = testGroup "Literals"
 [ QC.testProperty "Numeric literals" prop_numeric_literals ]

prop_numeric_literals :: Int -> Bool
prop_numeric_literals n = True

