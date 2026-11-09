module Main (main) where

import Data.Xch
import Text.Megaparsec
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Test.Xch"
  [testEmpty]

testEmpty :: TestTree
testEmpty = testCase "empty" $ parseMaybe pxch "<xch/>" @?= Just []
