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
    PrettyCOT t st d c e u fM -> maybe putStrLn writeFile fM $ show $ pretty $ rpt t st d c e u

render :: Bool -> COT -> String
render s c | s = show c
           | otherwise = show $ vsep $ map pretty c

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "Commitments of Traders"

data CLI = ParseCOT Bool (Maybe String)
  | PrettyCOT String String String String String String (Maybe String)

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "parse" $ info (ParseCOT <$> showSwitch <*> optional fileArg) $
      progDesc "Parse COT reports"
  , command "pretty" $ info prettyParser $ progDesc "Prettyprint COT report"
  ]

showSwitch :: Parser Bool
showSwitch = switch $ short 's' <> long "show" <> help "Show instance. Default pretty."

fileArg :: Parser String
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional input file otherwise stream STDIN"

prettyParser :: Parser CLI
prettyParser = PrettyCOT
  <$> strOption (long "title" <> value "Disaggregated Commitments of Traders" <> showDefault <> help "report title")
  <*> strOption (long "subtitle" <> value "All Futures Combined Positions" <> showDefault <> help "report subtitle")
  <*> strOption (long "date" <> help "report date")
  <*> strOption (long "contract" <> help "report contract")
  <*> strOption (long "exchange" <> help "report exchange")
  <*> strOption (long "under" <> help "underlying asset")
  <*> optional fileArg

rpt :: String -> String -> String -> String -> String -> String -> Rpt
rpt t st d c e u = Rpt{title=t,subtitle=st,date=d,repPos="Reportable Positions"
  , contract=c,exchange=e,uparens=False,underlying=u,code="",openInterest=0
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
