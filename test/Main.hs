module Main (main) where

import Data.Xch
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Megaparsec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain $ testGroup "Test.Xch"
  [testEmpty, testSVDat, testMsnEvt, testComStat, testAEDat, ptrip]

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

testAEDat :: TestTree
testAEDat = testCase "AEDat" $ do
  ptest "<xch><aedat><ae>foo</ae></aedat></xch>" [AEDat "foo" []]
  ptest "<xch><aedat><ae>bar</ae><actdur><act>4</act><dur>5</dur></actdur></aedat></xch>"
    [AEDat "bar" [ActDurTime 4 5]]
  ptest "<xch><aedat><ae>baz</ae><dfn>zoo</dfn></aedat></xch>"
    [AEDat "baz" [Dfn "zoo"]]
  ptest "<xch><aedat><ae>bar</ae>\
    \<loc><geo>San Diego</geo>\
    \  <lat><cord><dgs>34</dgs><mns>-2</mns><sds>3.14</sds></cord></lat>\
    \  <lon><cord><dgs>0</dgs><mns>55</mns><sds>1.0001</sds></cord></lon>\
    \  <alt>45</alt>\
    \</loc></aedat></xch>"
    [AEDat "bar" [LocGeo "San Diego" [(Cord 34 (-2) 3.14, Cord 0 55 1.0001)] $ Just 45]]

ptest :: String -> Xch -> Assertion
ptest i expt = case parse pxch "" i of
  Left  err -> assertFailure $ errorBundlePretty err
  Right act -> act @?= expt

ptrip :: TestTree
ptrip = testProperty "Trip" $ property $ do
  x <- forAll gxch
  tripping x (show . wxch) (parse pxch "")

gxch :: MonadGen m => m Xch
gxch = glist gxdat

gxdat :: MonadGen m => m XDat
gxdat = Gen.choice
  [ MsnEvt <$> gstr <*> glist gstr
  , ComStat <$> Gen.element [Excelnt, Good, Bad, Lost]
  ]

gint :: MonadGen m => m Integer
gint = Gen.integral_ $ Range.linearFrom 0 (-999999999999999) 999999999999999

gdbl :: MonadGen m => m Double
gdbl = Gen.double $ Range.linearFracFrom 0 (-999999999999999) 999999999999999

gstr :: MonadGen m => m String
gstr = esc <$> Gen.string rangeList Gen.alphaNum
  where
    esc [] = []
    esc ('\\':'<':rest) = "\\<" ++ esc rest
    esc (c:'<':rest) = [c] ++ "\\<" ++ esc rest
    esc ('<':rest) = "\\<" ++ esc rest
    esc (c:rest) = c : esc rest

glist :: MonadGen m => m a -> m [a]
glist = Gen.list rangeList

rangeList :: Range Int
rangeList = Range.linearFrom 0 0 256
