{-# LANGUAGE OverloadedStrings #-}

-- | Standard Guidance for Space Data Exchange
module Data.Xch (Xch, writeXch, XDat(..), SVDat(..), ComStat(..)
  , AEInfo(..), Cord(..)) where

import Data.Time.Clock.POSIX
import Prettyprinter

-- | exchange
type Xch = [XDat]

-- | render exchange data in XML format
writeXch :: Xch -> String
writeXch _ = show $ vsep
  [ "<xch>"
  , "</xch>"]

-- | space data
data XDat = SVDat [SVDat] | MsnEvt [String] | ComStat ComStat | AEDat [AEInfo]

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
  deriving (Eq, Read, Show)

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
