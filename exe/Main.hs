module Main (main) where

import Data.Version
import Data.Xch
import Options.Applicative
import Paths_xch
import qualified Text.Megaparsec as M

main :: IO ()
main = do
  cli <- customExecParser prefs' $ pinfo $ showVersion version
  case cli of
    ReadXch fM -> do
      i <- maybe getContents readFile fM
      putStrLn $ either M.errorBundlePretty show $ M.parse pxch "" i
    WriteXch xch fM -> maybe putStrLn writeFile fM $ show $ wxch xch

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "Space Data Exchange"

data CLI = ReadXch (Maybe FilePath) | WriteXch Xch (Maybe FilePath)

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "r" $ info (ReadXch <$>
    optional (fileArg "input file or stream STDIN")) $
    progDesc "Read space data"
  , command "w" $ info (WriteXch <$> many parseXDat <*>
    optional (fileArg "output file or stream STDOUT")) $
    progDesc "Write space data"
  ]

fileArg :: String -> Parser FilePath
fileArg h = strArgument $ metavar "FILE" <> completer (bashCompleter "file") <> help h

parseXDat :: Parser XDat
parseXDat = asum
  [ parserOptionGroup "state vector data" $ SVDat <$>
    strOption (long "state" <> metavar "STR" <> help "identifier") <*>
    (many . asum)
      [ fmap Time $ option auto $ long "time" <>
        help "POSIX time e.g. $(echo \"$(date +%s.%N)s\")"
      , Pos <$> parseNED "pos", Vel <$> parseNED "vel"]
  , parserOptionGroup "mission event triggers" $ MsnEvt <$>
    strOption (long "trig" <> metavar "STR" <> help "trigger") <*>
    many (strOption $ long "evt" <> metavar "STR" <> help "event")
  , parserOptionGroup "communication health status" $ ComStat <$> asum
    [ flag' Excelnt $ long "excellent" <> help "excellent connection"
    , flag' Good $ long "good" <> help "good connection"
    , flag' Bad $ long "bad" <> help "bad connection"
    , flag' Lost $ long "lost" <> help "lost connection"
    ]]

parseNED :: String -> Parser NED
parseNED pfx = NED
  <$> option auto (long (pfx ++ "-north") <> metavar "DBL" <> help "north coordinate")
  <*> option auto (long (pfx ++ "-east") <> metavar "DBL" <> help "east coordinate")
  <*> option auto (long (pfx ++ "-down") <> metavar "DBL" <> help "down coordinate")
