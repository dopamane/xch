module Main (main) where

import Data.Version
import Data.Xch
import Options.Applicative
import Paths_xch

main :: IO ()
main = do
  cli <- customExecParser prefs' $ pinfo $ showVersion version
  case cli of
    ReadXch{} -> error "not implemented"
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
    many (asum [])
  , parserOptionGroup "communication health status" $ ComStat <$> asum
    [ flag' Excelnt $ long "excelnt" <> help "excellent connection"
    , flag' Good $ long "good" <> help "good connection"
    , flag' Poor $ long "poor" <> help "poor connection"
    , flag' Lost $ long "lost" <> help "lost connection"
    ]]
