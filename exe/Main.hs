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
    PrettyCOT t st d c e up u co oi (p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) cf
      (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) (o0,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10)
      tt (t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) fM ->
        maybe putStrLn writeFile fM $ show $ pretty $
        Rpt{title=t,subtitle=st,date=d,repPos="Reportable Positions"
        , contract=c,exchange=e,uparens=up,underlying=u,code=co,openInterest=oi
        ,changesFrom=cf,totTraders=tt
        ,prod=Cat{posLong=p0, posShort=p1, posSpread=Nothing
          , changeLong=c0, changeShort=c1, changeSpread=Nothing
          , pctOpenLong=o0, pctOpenShort=o1, pctOpenSpread=Nothing
          , tradersLong=t0, tradersShort=t1, tradersSpread=Nothing}
        ,swap=Cat{posLong=p2, posShort=p3, posSpread=Just p4
          , changeLong=c2, changeShort=c3, changeSpread=Just c4
          , pctOpenLong=o2, pctOpenShort=o3, pctOpenSpread=Just o4
          , tradersLong=t2, tradersShort=t3, tradersSpread=t4}
        ,managed=Cat{posLong=p5, posShort=p6, posSpread=Just p7
          , changeLong=c5, changeShort=c6, changeSpread=Just c7
          , pctOpenLong=o5, pctOpenShort=o6, pctOpenSpread=Just o7
          , tradersLong=t5, tradersShort=t6, tradersSpread=t7}
        ,other=Cat{posLong=p8, posShort=p9, posSpread=Just p10
          , changeLong=c8, changeShort=c9, changeSpread=Just c10
          , pctOpenLong=o8, pctOpenShort=o9, pctOpenSpread=Just o10
          , tradersLong=t8, tradersShort=t9, tradersSpread=t10}
        }

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
    Integer (Row Integer) String (Row Integer) (Row Double) Integer
    (Row (Maybe Integer)) (Maybe String)

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
  <*> strOption (long "changes-from" <> help "date of last change")
  <*> option (eitherReader row) (long "changes" <> value zrow <> help "report changes, default zeros")
  <*> option (eitherReader row) (long "percents" <> value zrow <> help "percents of open interest")
  <*> option auto (long "total-traders" <> value 0 <> showDefault <> help "report total traders")
  <*> option (eitherReader trow) (long "traders" <> value nrow <> help "report traders, default dots")
  <*> optional fileArg

zrow :: Num a => Row a
zrow = (0,0,0,0,0,0,0,0,0,0,0)

nrow :: Row (Maybe a)
nrow = (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing
  ,Nothing,Nothing)

row :: Read a => String -> Either String (Row a)
row = rrow readEither

rrow :: Read a => (String -> Either String a) -> String -> Either String (Row a)
rrow r s = case words s of
  [w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10] -> (,,,,,,,,,,) <$> r w0 <*> r w1
    <*> r w2 <*> r w3 <*> r w4 <*> r w5 <*> r w6 <*> r w7 <*> r w8 <*>
    r w9 <*> r w10
  _ -> Left "expecting 11 row elements"

trow :: String -> Either String (Row (Maybe Integer))
trow = rrow readTrader
  where
    readTrader "." = Right Nothing
    readTrader w = Just <$> readEither w
