module Main (main) where

import Data.Xch
import Text.Megaparsec
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Test.Xch"
  [testEmpty, testSVDat, testMsnEvt, testComStat]

testEmpty :: TestTree
testEmpty = testCase "empty" $ do
  ptest "<xch/>" []
  ptest "<xch></xch>" []

testSVDat :: TestTree
testSVDat = testCaseSteps "SVDat" $ \step -> do
  step "state"
  ptest "<xch><svdat><state>foo</state></svdat></xch>" [SVDat "foo" []]
  ptest "<xch><svdat><state>foo bar</state></svdat></xch>" [SVDat "foo bar" []]
  ptest "<xch><svdat><state>\\<escape</state></svdat></xch>" [SVDat "<escape" []]
  step "time"
  ptest "<xch><svdat><state>foo</state><time>1794828361.374233416s</time></svdat></xch>"
    [SVDat "foo" [Time $ read "1794828361.374233416s"]]
  step "pos"
  ptest "<xch><svdat><state>foo</state><pos><ned>\
    \<north>3</north><east>-4</east><down>5</down>\
    \</ned></pos></svdat></xch>"
      [SVDat "foo" [Pos $ NED 3.0 (-4.0) 5.0]]
  ptest "<xch><svdat><state>foo</state><pos><ned>\
    \<north>3.1</north><east>-4.2</east><down>5.5</down>\
    \</ned></pos></svdat></xch>"
      [SVDat "foo" [Pos $ NED 3.1 (-4.2) 5.5]]
  step "vel"
  ptest "<xch><svdat><state>foo</state><vel><ned>\
    \<north>+3</north><east>4</east><down>5.2</down>\
    \</ned></vel></svdat></xch>"
      [SVDat "foo" [Vel $ NED 3.0 4.0 5.2]]
  ptest "<xch><svdat><state>foo</state><vel><ned>\
    \<north>3.1e5</north><east>- 4.2</east><down>5.5</down>\
    \</ned></vel></svdat></xch>"
      [SVDat "foo" [Vel $ NED 3.1e5 (-4.2) 5.5]]

testMsnEvt :: TestTree
testMsnEvt = testCase "MsnEvt" $ do
  ptest "<xch><misn><trig>foo</trig></misn></xch>" [MsnEvt "foo" []]
  ptest "<xch><misn><trig>bar</trig><evt>up</evt></misn></xch>" [MsnEvt "bar" ["up"]]
  ptest "<xch><misn><trig>baz</trig><evt>left</evt><evt>right</evt></misn></xch>"
    [MsnEvt "baz" ["left", "right"]]

testComStat :: TestTree
testComStat = testCase "ComStat" $ do
  ptest "<xch><com><excellent/></com></xch>" [ComStat Excelnt]
  ptest "<xch><com><good/></com></xch>" [ComStat Good]
  ptest "<xch><com><bad/></com></xch>" [ComStat Bad]
  ptest "<xch><com><lost/></com></xch>" [ComStat Lost]

ptest :: String -> Xch -> Assertion
ptest i expt = case parse pxch "" i of
  Left  err -> assertFailure $ errorBundlePretty err
  Right act -> act @?= expt
