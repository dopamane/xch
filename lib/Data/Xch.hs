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
wxdat xdat = intercalate "\n" $ case xdat of
  SVDat stte sv -> [lvl 2 ++ bgn "svdat", lvl 4 ++ stag "state" stte] ++
    map wsvdat sv ++ [lvl 2 ++ end "svdat"]
  MsnEvt trig es -> [lvl 2 ++ bgn "misn", lvl 4 ++ stag "trig" trig] ++
    [lvl 4 ++ stag "evt" e | e <- es] ++ [lvl 2 ++ end "misn"]
  ComStat s -> [lvl 2 ++ bgn "com", wcomstat s, lvl 2 ++ end "com"]
  AEDat idnt inf -> [lvl 2 ++ bgn "aedat", lvl 4 ++ stag "ae" idnt] ++
    map waeinf inf ++ [lvl 2 ++ end "aedat"]

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
  Time t -> lvl 4 ++ tag "time" t
  Pos ned -> intercalate "\n" [lvl 4 ++ bgn "pos", wned ned, lvl 4 ++ end "pos"]
  Vel ned -> intercalate "\n" [lvl 4 ++ bgn "vel", wned ned, lvl 4 ++ end "vel"]

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
  ActDurTime t d -> intercalate "\n" [lvl 4 ++ bgn "actdur", lvl 6 ++ tag "act" t
    , lvl 6 ++ tag "dur" d, lvl 4 ++ end "actdur"]
  LocGeo idnt cs altM -> intercalate "\n" $ concat
    [ [lvl 4 ++ bgn "loc"]
    , [lvl 6 ++ stag "geo" idnt]
    , [intercalate "\n" [ intercalate "\n"
        [ lvl 6 ++ bgn "lat"
        , wcord lat
        , lvl 6 ++ end "lat"
        , lvl 6 ++ bgn "lon"
        , wcord lon
        , lvl 6 ++ end "lon"
        ] | (lat, lon) <- cs] | not $ null cs]
    , case altM of
        Nothing -> []
        Just al -> [lvl 6 ++ tag "alt" al]
    , [lvl 4 ++ end "loc"]
    ]
  Dfn d -> lvl 4 ++ stag "dfn" d

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
wcord (Cord d m s) = intercalate "\n" [lvl 8 ++ bgn "cord"
  , lvl 10 ++ tag "dgs" d, lvl 10 ++ tag "mns" m
  , lvl 10 ++ tag "sds" s, lvl 8 ++ end "cord"]

pcord :: Parser Cord
pcord = ptag "cord" $ Cord <$> ptag "dgs" pint <*> ptag "mns" pint
  <*> ptag "sds" pdbl

-- | coordinate: north, east, down
data NED = NED{north :: Double, east :: Double, down :: Double}
  deriving (Eq, Read, Show)

wned :: NED -> String
wned (NED n e d) = intercalate "\n" [lvl 6 ++ bgn "ned", lvl 8 ++ tag "north" n
  , lvl 8 ++ tag "east" e, lvl 8 ++ tag "down" d, lvl 6 ++ end "ned"]

pned :: Parser NED
pned = ptag "ned" $ NED <$> ptag "north" pdbl <*> ptag "east" pdbl
  <*> ptag "down" pdbl

lvl :: Int -> String
lvl i = replicate i ' '

bgn :: String -> String
bgn t = "<" ++ t ++ ">"

end :: String -> String
end t = "</" ++ t ++ ">"

stag :: String -> String -> String
stag t s = bgn t ++ s ++ end t

tag :: Show a => String -> a -> String
tag t = stag t . show

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
pstag t = sym "<" *> sym t *> char '>' *> manyTill anySingle (pend t)
