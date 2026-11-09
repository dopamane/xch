module Main (main) where

import Data.Xch
import Text.Megaparsec
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Test.Xch"
  [testEmpty, testSVDat, testComStat]

testEmpty :: TestTree
testEmpty = testCase "empty" $ do
  parseMaybe pxch "<xch/>" @?= Just []
  parseMaybe pxch "<xch></xch>" @?= Just []

testSVDat :: TestTree
testSVDat = testCase "SVDat" $ do
  parseMaybe pxch "<xch><svdat><state>foo</state></svdat></xch>" @?= Just [SVDat "foo" []]
  parseMaybe pxch "<xch><svdat><state>foo bar</state></svdat></xch>" @?= Just [SVDat "foo bar" []]
  parseMaybe pxch "<xch><svdat><state>\\<escape</state></svdat></xch>" @?= Just [SVDat "<escape" []]

testComStat :: TestTree
testComStat = testCase "ComStat" $ do
  parseMaybe pxch "<xch><com><excellent/></com></xch>" @?= Just [ComStat Excelnt]
  parseMaybe pxch "<xch><com><good/></com></xch>" @?= Just [ComStat Good]
  parseMaybe pxch "<xch><com><bad/></com></xch>" @?= Just [ComStat Bad]
  parseMaybe pxch "<xch><com><lost/></com></xch>" @?= Just [ComStat Lost]
