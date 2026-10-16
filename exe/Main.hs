module Main (main) where

import Data.COT
import Data.Version
import Options.Applicative
import Paths_cot
import Text.Megaparsec

main :: IO ()
main = do
  cli <- customExecParser prefs' $ pinfo $ showVersion version
  case cli of
    ParseCOT fM -> do
      s <- maybe getContents readFile fM
      putStrLn $ either errorBundlePretty show $ parseCOT s
    PrettyCOT _ _ -> error "not implemented"

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "Commitments of Traders"

data CLI = ParseCOT (Maybe String) | PrettyCOT Rpt (Maybe String)

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "parse" $ info (ParseCOT <$> optional fileArg) $ progDesc "Parse COT reports"
  , command "pretty" $ info (PrettyCOT <$> rptParser <*> optional fileArg) $
      progDesc "Prettyprint COT report"
  ]

fileArg :: Parser String
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional input file otherwise stream STDIN"

rptParser :: Parser Rpt
rptParser = undefined
