module Main (main) where

import Data.Version
import Data.Xch
import Options.Applicative
import Paths_xchange

main :: IO ()
main = do
  cli <- customExecParser prefs' $ pinfo $ showVersion version
  case cli of
    ReadXch{} -> error "not implemented"
    WriteXch{} -> error "not implemented"

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "Space Data Exchange"

data CLI = ReadXch (Maybe FilePath) | WriteXch Xch (Maybe FilePath)

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "r" $ info (pure $ ReadXch Nothing) $ progDesc "Read space data"
  , command "w" $ info (pure $ WriteXch [] Nothing) $ progDesc "Write space data"
  ]
