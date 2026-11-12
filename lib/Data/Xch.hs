-- | Standard Guidance for Space Data Exchange
module Data.Xch (Xch, wxch, pxch, XDat(..), SVDat(..), ComStat(..)
  , AEInfo(..), Cord(..), NED(..)) where

import Data.Functor
import Data.List
import Data.Time.Clock.POSIX
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read hiding (choice)

-- | exchange
type Xch = [XDat]

-- | write exchange data in XML format
wxch :: Xch -> String
wxch [] = tag_ "xch"
wxch xs = unlines $ [bgn "xch"] ++ map wxdat xs ++ [end "xch"]

-- | parse exchange XML data
pxch :: Parser Xch
pxch = try (ptag_ "xch" $> []) <|> ptag "xch" (many pxdat)

-- | space data
data XDat = SVDat String [SVDat] | MsnEvt String [String] | ComStat ComStat
  | AEDat String [AEInfo]
  deriving (Eq, Read, Show)

wxdat :: XDat -> String
wxdat xdat = case xdat of
  SVDat stte sv -> unlines $ [bgn "svdat", tag "state" stte] ++ map wsvdat sv ++ [end "svdat"]
  MsnEvt trig es -> intercalate "\n" $
    ["  " ++ bgn "misn", "    " ++ tag "trig" trig] ++
      map (mappend "    " . tag "evt") es ++ ["  " ++ end "misn"]
  ComStat s -> intercalate "\n" ["  " ++ bgn "com", wcomstat s, "  " ++ end "com"]
  AEDat idnt inf -> unlines $
    [ bgn "aedat", tag "ae" idnt] ++ map waeinf inf ++ [end "aedat"]

pxdat :: Parser XDat
pxdat = choice [try $ ptag "svdat" $ SVDat <$> pstag "state" <*> many psvdat
  , try $ ptag "misn" $ MsnEvt <$> pstag "trig" <*> many (try $ pstag "evt")
  , try $ ComStat <$> ptag "com" pcomstat
  , try $ ptag "aedat" $ AEDat <$> pstag "ae" <*> many paeinf]

-- | State vector data
data SVDat
  = -- | timestamp
    Time POSIXTime
  | -- | position
    Pos NED
  | -- | velocity
    Vel NED
  deriving (Eq, Read, Show)

wsvdat :: SVDat -> String
wsvdat s = case s of
  Time t -> tag "time" t
  Pos ned -> unlines [bgn "pos", wned ned, end "pos"]
  Vel ned -> unlines [bgn "vel", wned ned, end "vel"]

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

wcomstat :: ComStat -> String
wcomstat s = mappend "    " $ tag_ $ case s of
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

waeinf :: AEInfo -> String
waeinf ae = case ae of
  ActDurTime t d -> unlines [bgn "actdur", tag "act" t, tag "dur" d, end "actdur"]
  LocGeo idnt cs altM -> unlines
    [ bgn "loc"
    , tag "geo" idnt
    , unlines [ unlines
        [ bgn "lat"
        , wcord lat
        , end "lat"
        , bgn "lon"
        , wcord lon
        , end "lon"
        ] | (lat, lon) <- cs]
    , case altM of
        Nothing -> ""
        Just al -> tag "alt" al
    , end "loc"
    ]
  Dfn d -> tag "dfn" d

paeinf :: Parser AEInfo
paeinf = choice
  [ try $ ptag "actdur" $ ActDurTime <$> ptag "act" pint <*> ptag "dur" pint
  , try $ ptag "loc" $ LocGeo <$> pstag "geo"
    <*> many (try $ (,) <$> ptag "lat" pcord <*> ptag "lon" pcord)
    <*> optional (try $ ptag "alt" pint)
  , try $ Dfn <$> pstag "dfn"
  ]

-- | coordinate: degrees, minutes, seconds
data Cord = Cord{dgs :: Integer, mns :: Integer, sds :: Double}
  deriving (Eq, Read, Show)

wcord :: Cord -> String
wcord (Cord d m s) = unlines [bgn "cord", tag "dgs" d, tag "mns" m
  , tag "sds" s, end "cord"]

pcord :: Parser Cord
pcord = ptag "cord" $ Cord <$> ptag "dgs" pint <*> ptag "mns" pint
  <*> ptag "sds" pdbl

-- | coordinate: north, east, down
data NED = NED{north :: Double, east :: Double, down :: Double}
  deriving (Eq, Read, Show)

wned :: NED -> String
wned (NED n e d) = unlines [bgn "ned", tag "north" n, tag "east" e
  , tag "down" d, end "ned"]

pned :: Parser NED
pned = ptag "ned" $ NED <$> ptag "north" pdbl <*> ptag "east" pdbl
  <*> ptag "down" pdbl

bgn :: String -> String
bgn t = "<" ++ t ++ ">"

end :: String -> String
end t = "</" ++ t ++ ">"

tag :: Show a => String -> a -> String
tag t a  = bgn t ++ show a ++ end t

tag_ :: String -> String
tag_ t = "<" ++ t ++ "/>"

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
pdbl = try (L.signed sc $ lexe L.float) <|> L.signed sc (lexe L.decimal)

pint :: Parser Integer
pint = L.signed sc $ lexe L.decimal

pstag :: String -> Parser String
pstag t = sym "<" *> sym t *> char '>' *> manyTill L.charLiteral (pend t)
