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
    PrettyCOT ty d c e u co oi (p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) cf
      (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) (o0,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10)
      tt (t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) fM ->
        maybe putStrLn writeFile fM $ show $ pretty $
        Rpt{rptType=ty,date=d,contract=c,exchange=e,underlying=u
          ,code=co,openInterest=oi,changesFrom=cf,totTraders=tt
        ,prod=Cat{posLong=p0, posShort=p1, posSpread=Nothing
          , changeLong=c0, changeShort=c1, changeSpread=Nothing
          , pctOpenLong=o0, pctOpenShort=o1, pctOpenSpread=Nothing
          , tradersLong=t0, tradersShort=t1, tradersSpread=Nothing}
        ,swap=Cat{posLong=p2, posShort=p3, posSpread=p4
          , changeLong=c2, changeShort=c3, changeSpread=c4
          , pctOpenLong=o2, pctOpenShort=o3, pctOpenSpread=o4
          , tradersLong=t2, tradersShort=t3, tradersSpread=t4}
        ,managed=Cat{posLong=p5, posShort=p6, posSpread=p7
          , changeLong=c5, changeShort=c6, changeSpread=c7
          , pctOpenLong=o5, pctOpenShort=o6, pctOpenSpread=o7
          , tradersLong=t5, tradersShort=t6, tradersSpread=t7}
        ,other=Cat{posLong=p8, posShort=p9, posSpread=p10
          , changeLong=c8, changeShort=c9, changeSpread=c10
          , pctOpenLong=o8, pctOpenShort=o9, pctOpenSpread=o10
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
  | PrettyCOT RptType String String String String String
    Integer (Row (Maybe Integer)) String (Row (Maybe Integer)) (Row (Maybe Double)) Integer
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
  <$> (flag' Futes (long "futes" <> help "futures only report type") <|>
        flag' OptsAndFutes (long "opts-futes" <> help "options and futures report type"))
  <*> strOption (long "date" <> help "report date")
  <*> strOption (long "contract" <> help "report contract")
  <*> strOption (long "exchange" <> help "report exchange")
  <*> strOption (long "under" <> help "underlying asset")
  <*> strOption (long "code" <> value "000000" <> showDefault <> help "CFTC code")
  <*> option auto (long "open-interest" <> value 0 <> showDefault <> help "report open interest")
  <*> option (eitherReader row) (long "positions" <> value nrow <> help "report positions, default dots")
  <*> strOption (long "changes-from" <> help "date of last change")
  <*> option (eitherReader row) (long "changes" <> value nrow <> help "report changes, default dots")
  <*> option (eitherReader row) (long "percents" <> value nrow <> help "percents of open interest, default dots")
  <*> option auto (long "total-traders" <> value 0 <> showDefault <> help "report total traders")
  <*> option (eitherReader row) (long "traders" <> value nrow <> help "report traders, default dots")
  <*> optional fileArg

nrow :: Row (Maybe a)
nrow = (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing
  ,Nothing,Nothing)

row :: Read a => String -> Either String (Row (Maybe a))
row = rrow readVal
  where
    readVal "." = Right Nothing
    readVal v = Just <$> readEither v

rrow :: Read a => (String -> Either String a) -> String -> Either String (Row a)
rrow r s = case words s of
  [w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10] -> (,,,,,,,,,,) <$> r w0 <*> r w1
    <*> r w2 <*> r w3 <*> r w4 <*> r w5 <*> r w6 <*> r w7 <*> r w8 <*>
    r w9 <*> r w10
  _ -> Left "expecting 11 row elements"
