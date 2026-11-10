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
  parseMaybe pxch "<xch><svdat><state>foo</state><time>1794828361.374233416s</time></svdat></xch>"
    @?= Just [SVDat "foo" [Time $ read "1794828361.374233416s"]]
  parseMaybe pxch "<xch><svdat><state>foo</state><pos><ned>\
    \<north>3</north><east>-4</east><down>5</down>\
    \</ned></pos></svdat></xch>" @?=
      Just [SVDat "foo" [Pos $ NED 3.0 (-4.0) 5.0]]
  parseMaybe pxch "<xch><svdat><state>foo</state><pos><ned>\
    \<north>3.1</north><east>-4.2</east><down>5.5</down>\
    \</ned></pos></svdat></xch>" @?=
      Just [SVDat "foo" [Pos $ NED 3.1 (-4.2) 5.5]]

testComStat :: TestTree
testComStat = testCase "ComStat" $ do
  parseMaybe pxch "<xch><com><excellent/></com></xch>" @?= Just [ComStat Excelnt]
  parseMaybe pxch "<xch><com><good/></com></xch>" @?= Just [ComStat Good]
  parseMaybe pxch "<xch><com><bad/></com></xch>" @?= Just [ComStat Bad]
  parseMaybe pxch "<xch><com><lost/></com></xch>" @?= Just [ComStat Lost]
