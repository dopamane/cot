module Main (main) where

import Data.COT
import Data.Version
import Options.Applicative
import Paths_cot
import Prettyprinter
import Text.Megaparsec

main :: IO ()
main = do
  cli <- customExecParser prefs' $ pinfo $ showVersion version
  case cli of
    ParseCOT s fM -> do
      i <- maybe getContents readFile fM
      putStrLn $ either errorBundlePretty (render s) $ parseCOT i
    PrettyCOT _ _ -> error "not implemented"

render :: Bool -> COT -> String
render s c | s = show c
           | otherwise = show $ vsep $ map pretty c

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "Commitments of Traders"

data CLI = ParseCOT Bool (Maybe String) | PrettyCOT Rpt (Maybe String)

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "parse" $ info (ParseCOT <$> showSwitch <*> optional fileArg) $
      progDesc "Parse COT reports"
  , command "pretty" $ info (PrettyCOT <$> rptParser <*> optional fileArg) $
      progDesc "Prettyprint COT report"
  ]

showSwitch :: Parser Bool
showSwitch = switch $ short 's' <> long "show" <> help "Show instance. Default pretty."

fileArg :: Parser String
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional input file otherwise stream STDIN"

rptParser :: Parser Rpt
rptParser = undefined
