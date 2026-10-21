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
    PrettyCOT r fM -> maybe putStrLn writeFile fM $ show $ pretty r

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
rptParser = fmap rpt $ (,,)
  <$> strOption (long "title" <> value "Disaggregated Commitments of Traders" <> showDefault <> help "report title")
  <*> strOption (long "subtitle" <> value "All Futures Combined Positions" <> showDefault <> help "report subtitle")
  <*> strOption (long "date" <> help "report date")

rpt :: (String,String,String) -> Rpt
rpt (t,st,d) = Rpt{title=t,subtitle=st,date=d,repPos="Reportable Positions"
  , contract="",exchange="",uparens=False,underlying="",code="",openInterest=0
  ,changesFrom="",totTraders=0,prod=prod',swap=swap',managed=managed',
  other=other'}
  where
    prod'=Cat{posLong=0, posShort=0, posSpread=Nothing
      , changeLong=0, changeShort=0, changeSpread=Nothing
      , pctOpenLong=0.0, pctOpenShort=0.0, pctOpenSpread=Nothing
      , tradersLong=Nothing, tradersShort=Nothing, tradersSpread=Nothing}
    swap'=Cat{posLong=0, posShort=0, posSpread=Just 0
      , changeLong=0, changeShort=0, changeSpread=Just 0
      , pctOpenLong=0.0, pctOpenShort=0.0, pctOpenSpread=Just 0
      , tradersLong=Nothing, tradersShort=Nothing, tradersSpread=Nothing}
    managed'=Cat{posLong=0, posShort=0, posSpread=Just 0
      , changeLong=0, changeShort=0, changeSpread=Just 0
      , pctOpenLong=0.0, pctOpenShort=0.0, pctOpenSpread=Just 0
      , tradersLong=Nothing, tradersShort=Nothing, tradersSpread=Nothing}
    other'=Cat{posLong=0, posShort=0, posSpread=Just 0
      , changeLong=0, changeShort=0, changeSpread=Just 0
      , pctOpenLong=0.0, pctOpenShort=0.0, pctOpenSpread=Just 0
      , tradersLong=Nothing, tradersShort=Nothing, tradersSpread=Nothing}
