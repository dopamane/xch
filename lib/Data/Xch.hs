-- | Standard Guidance for Space Data Exchange
module Data.Xch (Xch, wxch, XDat(..), SVDat(..), ComStat(..)
  , AEInfo(..), Cord(..)) where

import Data.List
import Data.Time.Clock.POSIX

-- | exchange
type Xch = [XDat]

-- | write exchange data in XML format
wxch :: Xch -> String
wxch = vtag "xch" . map wxdat

-- | space data
data XDat = SVDat [SVDat] | MsnEvt [String] | ComStat ComStat | AEDat [AEInfo]

wxdat :: XDat -> String
wxdat xdat = case xdat of
  SVDat sv -> vtag "svdat" $ wsvdat <$> sv
  MsnEvt{} -> ""
  ComStat s -> tag "com" $ wcomstat s
  AEDat{} -> ""

-- | State vector data
data SVDat
  = -- | component
    Cmpnt String
  | -- | timestamp
    Time POSIXTime
  | -- | position
    Pos NED
  | -- | velocity
    Vel NED

wsvdat :: SVDat -> String
wsvdat s = case s of
  Cmpnt c -> tag "cmpnt" c
  Time{} -> ""
  Pos{} -> ""
  Vel{} -> ""

-- | Communication health status
data ComStat = Excelnt | Good | Poor | Lost
  deriving (Bounded, Enum, Eq, Read, Show)

wcomstat :: ComStat -> String
wcomstat s = tag_ $ case s of
  Excelnt -> "excellent"
  Good -> "good"
  Poor -> "poor"
  Lost -> "lost"

-- | Anomaly and emergency info
data AEInfo = Ident String
  | -- | activation time and duration
    ActDurTime Integer Integer
  | -- | location\/geometry: bounds @(lat,lon)@ and altitude
    LocGeo [(Cord, Cord)] (Maybe Integer)
  | -- | definition of anomaly
    Dfn String

-- | coordinate: degrees, minutes, seconds
data Cord = Cord{dgs :: Integer, mns :: Integer, sds :: Integer}
  deriving (Eq, Read, Show)

-- | coordinate: north, east, down
data NED = NED{north :: Double, east :: Double, down :: Double}
  deriving (Eq, Read, Show)

bgn :: String -> String
bgn t = "<" ++ t ++ ">"

end :: String -> String
end t = "</" ++ t ++ ">"

tag :: String -> String -> String
tag t "" = "<" ++ t ++ "/>"
tag t cs = bgn t ++ cs ++ end t

tag_ :: String -> String
tag_ t = tag t ""

nlines :: [String] -> String
nlines = intercalate "\n"

vtag :: String -> [String] -> String
vtag t [] = tag_ t
vtag t ts = nlines [bgn t, nlines $ ("  " ++) <$> ts, end t]
