-- | Standard Guidance for Space Data Exchange
module Data.Xch (Xch, writeXch, XDat(..), SVDat(..), ComStat(..)
  , AEInfo(..), Cord(..)) where

import Data.List
import Data.Time.Clock.POSIX

-- | exchange
type Xch = [XDat]

-- | render exchange data in XML format
writeXch :: Xch -> String
writeXch xch = unlines ["<xch>", intercalate "\n" $ ("  " ++) . writeXDat <$> xch, "</xch>"]

-- | space data
data XDat = SVDat [SVDat] | MsnEvt [String] | ComStat ComStat | AEDat [AEInfo]

writeXDat :: XDat -> String
writeXDat xdat = case xdat of
  SVDat{} -> ""
  MsnEvt{} -> ""
  ComStat s -> tag "com" $ writeComStat s
  AEDat{} -> ""

-- | State vector data
data SVDat
  = -- | component
    Cmpnt String
  | Time POSIXTime
  | -- | position
    Pos
  | -- | velocity
    Vel

-- | Communication health status
data ComStat = Excelnt | Good | Poor | Lost
  deriving (Bounded, Enum, Eq, Read, Show)

writeComStat :: ComStat -> String
writeComStat s = case s of
  Excelnt -> tag_ "excellent"
  Good -> tag_ "good"
  Poor -> tag_ "poor"
  Lost -> tag_ "lost"

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

tag :: String -> String -> String
tag tdef "" = "<" ++ tdef ++ "/>"
tag tdef cs = "<" ++ tdef ++ ">" ++ cs ++ "<" ++ tdef ++ "/>"

tag_ :: String -> String
tag_ tdef = tag tdef ""
