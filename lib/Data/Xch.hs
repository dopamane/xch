-- | Standard Guidance for Space Data Exchange
module Data.Xch (Xch, wxch, pxch, XDat(..), SVDat(..), ComStat(..)
  , AEInfo(..), Cord(..), NED(..)) where

import Data.Functor
import Data.Time.Clock.POSIX
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read hiding (choice)
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
data XDat = SVDat String [SVDat] | MsnEvt String [String] | ComStat ComStat
  | AEDat String [AEInfo]
  deriving (Eq, Read, Show)

wxdat :: XDat -> Doc ann
wxdat xdat = case xdat of
  SVDat stte sv -> vtag "svdat" $ tag "state" [pretty stte] : map wsvdat sv
  MsnEvt trig es -> vtag "misn" $ tag "trig" [pretty trig] : [tag "evt" [pretty e] | e <- es]
  ComStat s -> tag "com" [wcomstat s]
  AEDat idnt inf -> vtag "aedat" $ tag "ae" [pretty idnt] : map waeinf inf

pxdat :: Parser XDat
pxdat = choice [try $ ptag "svdat" $ SVDat <$> ptag "state" pstr <*> many psvdat
  , try $ ptag "misn" $ MsnEvt <$> ptag "trig" pstr <*> many (try $ ptag "evt" pstr)
  , try $ ComStat <$> ptag "com" pcomstat]

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

psvdat :: Parser SVDat
psvdat = choice
  [ try $ ptag "time" $ Time <$> ptime, try $ ptag "pos" $ Pos <$> pned
  , try $ ptag "vel" $ Vel <$> pned]

ptime :: Parser POSIXTime
ptime = do
  ts <- some digitChar
  p  <- sym "."
  ns <- some digitChar
  s  <- sym "s"
  either fail return $ readEither $ ts ++ p ++ ns ++ s

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
  , try $ ptag_ "bad" $> Bad, try $ ptag_ "lost" $> Lost]

-- | Anomaly and emergency info
data AEInfo
  = -- | activation time and duration
    ActDurTime Integer Integer
  | -- | location\/geometry: name, bounds @(lat,lon)@ and altitude
    LocGeo String [(Cord, Cord)] (Maybe Integer)
  | -- | definition of anomaly
    Dfn String
  deriving (Eq, Read, Show)

waeinf :: AEInfo -> Doc ann
waeinf ae = case ae of
  ActDurTime t d -> vtag "actdur" [tag "act" [pretty t], tag "dur" [pretty d]]
  LocGeo idnt cs altM -> vtag "loc" $ concat
    [ [tag "geo" [pretty idnt]]
    , concat [[vtag "lat" [wcord lat], vtag "lon" [wcord lon]] | (lat, lon) <- cs]
    , maybe [] (pure . tag "alt" . pure . pretty) altM]
  Dfn d -> tag "dfn" [pretty d]

paeinf :: Parser AEInfo
paeinf = choice
  [ try $ ptag "actdur" $ ActDurTime <$> ptag "act" pint <*> ptag "dur" pint
  , try $ ptag "loc" $ LocGeo <$> ptag "geo" pstr <*> many undefined
    <*> optional (ptag "alt" pint)
  ]

-- | coordinate: degrees, minutes, seconds
data Cord = Cord{dgs :: Integer, mns :: Integer, sds :: Double}
  deriving (Eq, Read, Show)

wcord :: Cord -> Doc ann
wcord (Cord d m s) = vtag "cord" [tag "dgs" [pretty d], tag "mns" [pretty m]
  , tag "sds" [pretty s]]

pcord :: Parser Cord
pcord = ptag "cord" $ Cord <$> ptag "dgs" pint <*> ptag "mns" pint
  <*> ptag "sds" pdbl

-- | coordinate: north, east, down
data NED = NED{north :: Double, east :: Double, down :: Double}
  deriving (Eq, Read, Show)

wned :: NED -> Doc ann
wned (NED n e d) = vtag "ned" [tag "north" [pretty $ show n]
  , tag "east" [pretty $ show e], tag "down" [pretty $ show d]]

pned :: Parser NED
pned = ptag "ned" $ NED <$> ptag "north" pdbl <*> ptag "east" pdbl
  <*> ptag "down" pdbl

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

lexe :: Parser a -> Parser a
lexe = L.lexeme sc

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

pdbl :: Parser Double
pdbl = L.signed sc $ lexe L.float

pint :: Parser Integer
pint = L.signed sc $ lexe L.decimal

pstr :: Parser String
pstr = lexe $ many $ anySingleBut '<'
