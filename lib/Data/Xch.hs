-- | Standard Guidance for Space Data Exchange
module Data.Xch (Xch, XDat(..), SVDat(..), ComStat(..), AEInfo(..)) where

-- | exchange
type Xch = [XDat]

-- | space data
data XDat
  = SVDat
      -- | state identifier
      String
      [SVDat]
  | MsnEvt [String] | ComStat ComStat | AEDat [AEInfo]

-- | State vector data
data SVDat = Time | Pos | Vel

-- | Communication health status
data ComStat = Connect | Degrade | Disconn

-- | Anomaly and emergency info
data AEInfo = Ident String
  | -- | activation time and duration
    ActDurTime
  | -- | location\/geometry
    LocGeo
  | -- | definition of anomaly
    Dfn
