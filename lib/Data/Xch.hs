-- | Standard Guidance for Space Data Exchange
module Data.Xch (Xch, wxch, pxch, XDat(..), SVDat(..), ComStat(..)
  , AEInfo(..), Cord(..), NED(..)) where

import Data.Functor
import Data.Time.Clock.POSIX
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prettyprinter

-- | exchange
type Xch = [XDat]

-- | write exchange data in XML format
wxch :: Xch -> Doc ann
wxch = vtag "xch" . map wxdat

-- | parse exchange XML data
pxch :: Parser Xch
pxch = try (ptag_ "xch" $> []) <|> ptag "xch" (many pxdat)

-- | space data
data XDat = SVDat String [SVDat] | MsnEvt [String] | ComStat ComStat | AEDat [AEInfo]
  deriving (Eq, Read, Show)

wxdat :: XDat -> Doc ann
wxdat xdat = case xdat of
  SVDat ident sv -> vtag "svdat" $ tag "state" [pretty ident] : map wsvdat sv
  MsnEvt{} -> mempty
  ComStat s -> tag "com" [wcomstat s]
  AEDat{} -> mempty

pxdat :: Parser XDat
pxdat = choice [try $ ComStat <$> ptag "com" pcomstat]

-- | State vector data
data SVDat
  = -- | timestamp
    Time POSIXTime
  | -- | position
    Pos NED
  | -- | velocity
    Vel NED
  deriving (Eq, Read, Show)

wsvdat :: SVDat -> Doc ann
wsvdat s = case s of
  Time t -> tag "time" [pretty $ show t]
  Pos ned -> vtag "pos" [wned ned]
  Vel ned -> vtag "vel" [wned ned]

-- | Communication health status
data ComStat = Excelnt | Good | Bad | Lost
  deriving (Bounded, Enum, Eq, Read, Show)

wcomstat :: ComStat -> Doc ann
wcomstat s = tag_ $ case s of
  Excelnt -> "excellent"
  Good -> "good"
  Bad -> "bad"
  Lost -> "lost"

pcomstat :: Parser ComStat
pcomstat = choice [try $ ptag_ "excellent" $> Excelnt, try $ ptag_ "good" $> Good
  , try $ ptag_ "bad" $> Bad, ptag_ "lost" $> Lost]

-- | Anomaly and emergency info
data AEInfo = Ident String
  | -- | activation time and duration
    ActDurTime Integer Integer
  | -- | location\/geometry: bounds @(lat,lon)@ and altitude
    LocGeo [(Cord, Cord)] (Maybe Integer)
  | -- | definition of anomaly
    Dfn String
  deriving (Eq, Read, Show)

-- | coordinate: degrees, minutes, seconds
data Cord = Cord{dgs :: Integer, mns :: Integer, sds :: Double}
  deriving (Eq, Read, Show)

-- | coordinate: north, east, down
data NED = NED{north :: Double, east :: Double, down :: Double}
  deriving (Eq, Read, Show)

wned :: NED -> Doc ann
wned (NED n e d) = vtag "ned" [tag "north" [pretty $ show n]
  , tag "east" [pretty $ show e], tag "down" [pretty $ show d]]

bgn :: String -> Doc ann
bgn t = pretty $ "<" ++ t ++ ">"

end :: String -> Doc ann
end t = pretty $ "</" ++ t ++ ">"

tag :: String -> [Doc ann] -> Doc ann
tag t [] = pretty $ "<" ++ t ++ "/>"
tag t cs = bgn t <> hcat cs <> end t

tag_ :: String -> Doc ann
tag_ t = tag t []

vtag :: String -> [Doc ann] -> Doc ann
vtag t [] = tag_ t
vtag t ts = vsep [bgn t, vsep $ indent 2 <$> ts, end t]

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty $ L.skipBlockComment "<!--" "-->"

lex :: Parser a -> Parser a
lex = L.lexeme sc

sym :: String -> Parser String
sym = L.symbol sc

pbgn :: String -> Parser String
pbgn = between (sym "<") (sym ">") . sym

pend :: String -> Parser String
pend = between (sym "</") (sym ">") . sym

ptag :: String -> Parser a -> Parser a
ptag t = between (pbgn t) (pend t)

ptag_ :: String -> Parser String
ptag_ = between (sym "<") (sym "/>") . sym
