module Main (main) where

import Data.COT
import Data.Version
import Options.Applicative
import Paths_cot
import Prettyprinter
import Text.Megaparsec hiding (option)
import Text.Read

main :: IO ()
main = do
  cli <- customExecParser prefs' $ pinfo $ showVersion version
  case cli of
    ParseCOT s fM -> do
      i <- maybe getContents readFile fM
      putStrLn $ either errorBundlePretty (render s) $ parseCOT i
    PrettyCOT t st d c e up u co oi ps fM -> maybe putStrLn writeFile fM $ show $
      pretty $ rpt t st d c e up u co oi ps

render :: Bool -> COT -> String
render s c | s = show c
           | otherwise = show $ vsep $ map pretty c

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "Commitments of Traders"

data CLI = ParseCOT Bool (Maybe String)
  | PrettyCOT String String String String String Bool String String
    Integer (Row Integer) (Maybe String)

type Row a = (a,a,a,a,a,a,a,a,a,a,a)

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
  <*> switch (long "parens" <> help "parenthesize underlying asset")
  <*> strOption (long "under" <> help "underlying asset")
  <*> strOption (long "code" <> value "000000" <> showDefault <> help "CFTC code")
  <*> option auto (long "open-interest" <> value 0 <> showDefault <> help "report open interest")
  <*> option (eitherReader row) (long "positions" <> value zrow <> help "report positions, default zeros")
  <*> optional fileArg

zrow :: Num a => Row a
zrow = (0,0,0,0,0,0,0,0,0,0,0)

row :: Read a => String -> Either String (Row a)
row s = case words s of
  [w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10] -> (,,,,,,,,,,) <$> readEither w0
    <*> readEither w1 <*> readEither w2 <*> readEither w3 <*> readEither w4 <*>
    readEither w5 <*> readEither w6 <*> readEither w7 <*> readEither w8 <*>
    readEither w9 <*> readEither w10
  _ -> Left "expecting 11 row elements"

rpt :: String -> String -> String -> String -> String -> Bool -> String
  -> String -> Integer -> Row Integer -> Rpt
rpt t st d c e up u co oi (p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) =
  Rpt{title=t,subtitle=st,date=d,repPos="Reportable Positions"
  , contract=c,exchange=e,uparens=up,underlying=u,code=co,openInterest=oi
  ,changesFrom="",totTraders=0,prod=prod',swap=swap',managed=managed',
  other=other'}
  where
    prod'=Cat{posLong=p0, posShort=p1, posSpread=Nothing
      , changeLong=0, changeShort=0, changeSpread=Nothing
      , pctOpenLong=0.0, pctOpenShort=0.0, pctOpenSpread=Nothing
      , tradersLong=Nothing, tradersShort=Nothing, tradersSpread=Nothing}
    swap'=Cat{posLong=p2, posShort=p3, posSpread=Just p4
      , changeLong=0, changeShort=0, changeSpread=Just 0
      , pctOpenLong=0.0, pctOpenShort=0.0, pctOpenSpread=Just 0
      , tradersLong=Nothing, tradersShort=Nothing, tradersSpread=Nothing}
    managed'=Cat{posLong=p5, posShort=p6, posSpread=Just p7
      , changeLong=0, changeShort=0, changeSpread=Just 0
      , pctOpenLong=0.0, pctOpenShort=0.0, pctOpenSpread=Just 0
      , tradersLong=Nothing, tradersShort=Nothing, tradersSpread=Nothing}
    other'=Cat{posLong=p8, posShort=p9, posSpread=Just p10
      , changeLong=0, changeShort=0, changeSpread=Just 0
      , pctOpenLong=0.0, pctOpenShort=0.0, pctOpenSpread=Just 0
      , tradersLong=Nothing, tradersShort=Nothing, tradersSpread=Nothing}
