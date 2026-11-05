module Data.Xch (Xch(..), SVDat(..), ComStat(..), AEInfo(..)) where

data Xch = Xch{ svDat :: [(String, [SVDat])], msnEvt :: [String]
  , comStat :: ComStat, aeInfo :: [AEInfo] }

-- | State vector data
data SVDat = Time | Pos | Vel

-- | Communication health status
data ComStat = Connect | Degrade | Disconn

-- | Anomaly and emergency info
data AEInfo = AEInfo
