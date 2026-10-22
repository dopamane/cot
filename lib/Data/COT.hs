{-# LANGUAGE OverloadedStrings #-}

module Data.COT (COT, RptType(..), Rpt(..), Cat(..), parseCOT) where

import Control.Monad
import Data.Functor
import Data.Void
import Numeric
import Prettyprinter hiding (line)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type COT = [Rpt]

data RptType = Futes | OptsAndFutes
  deriving (Eq, Read, Show)

instance Pretty RptType where
  pretty Futes = "All Futures Combined Positions"
  pretty OptsAndFutes = "Options and Futures Combined Positions"

data Rpt = Rpt{rptType :: RptType, date :: String, contract :: String
  , exchange :: String, underlying :: String, code :: String
  , openInterest :: Integer, changesFrom :: String, totTraders :: Integer
  , prod :: Cat, swap :: Cat, managed :: Cat, other :: Cat}
  deriving (Eq, Read, Show)

pwidth :: Num a => a
pwidth = 112

cwidth :: Num a => a
cwidth = 9

instance Pretty Rpt where
  pretty r@Rpt{prod=p,swap=s,managed=m,other=o} = vsep
    [ let pad = case rptType r of
            Futes -> ""
            OptsAndFutes -> " "
          info = "Disaggregated Commitments of Traders-" ++ pad ++
            show (pretty $ rptType r) ++ " as of " ++ date r
      in pretty $ info ++ replicate (101 - length info) ' '
    , ":                                             Reportable Positions                                             :"
    , ":------------------------------------------------------------------------------------------------------------- :"
    , ": Producer/Merchant :                             :                             :                              :"
    , ":  Processor/User   :        Swap Dealers         :        Managed Money        :      Other Reportables       :"
    , ":   Long  :  Short  :   Long  :  Short  :Spreading:   Long  :  Short  :Spreading:   Long  :  Short  :Spreading :"
    , pretty $ replicate pwidth '-'
    , let info = contract r ++ " - " ++ exchange r ++ "   " ++ underlying r
          pad = pwidth - length info - 1
      in pretty $ info ++ replicate pad ' ' ++ ":"
    , let oi = snum $ openInterest r
          info = "CFTC Code #" ++ code r ++ replicate 52 ' ' ++ "Open Interest is " ++ replicate (9 - length oi) ' ' ++ oi
          pad = pwidth - length info - 1
      in pretty $ info ++ replicate pad ' ' ++ ":"
    , ": Positions                                                                                                    :"
    , prow [snum $ posLong p, snum $ posShort p
      , snum $ posLong s, snum $ posShort s, maybe "." snum $ posSpread s
      , snum $ posLong m, snum $ posShort m, maybe "." snum $ posSpread m
      , snum $ posLong o, snum $ posShort o, maybe "." snum $ posSpread o]
    , ":                                                                                                              :"
    , let info = "Changes from:" ++ replicate 5 ' ' ++ changesFrom r
      in ":" <+> pretty info <> pretty (replicate (pwidth - length info - 3) ' ') <> ":"
    , prow $ maybe "." snum <$> [changeLong p, changeShort p
      , changeLong s, changeShort s, changeSpread s
      , changeLong m, changeShort m, changeSpread m
      , changeLong o, changeShort o, changeSpread o]
    , ":                                                                                                              :"
    , ": Percent of Open Interest Represented by Each Category of Trader                                              :"
    , prow [ sdub $ pctOpenLong p, sdub $ pctOpenShort p
      , sdub $ pctOpenLong s, sdub $ pctOpenShort s, maybe "." sdub $ pctOpenSpread s
      , sdub $ pctOpenLong m, sdub $ pctOpenShort m, maybe "." sdub $ pctOpenSpread m
      , sdub $ pctOpenLong o, sdub $ pctOpenShort o, maybe "." sdub $ pctOpenSpread o]
    , ":                                                                                                              :"
    , let tot = snum $ totTraders r
      in ": Number of Traders in Each Category                                  Total Traders:"
           <> pretty (replicate (6 - length tot) ' ' ++ tot ++ replicate (cwidth * 2 + 2) ' ') <+> ":"
    , prow $ maybe "." snum <$> [tradersLong p, tradersShort p
      , tradersLong s, tradersShort s, tradersSpread s
      , tradersLong m, tradersShort m, tradersSpread m
      , tradersLong o, tradersShort o, tradersSpread o]
    , pretty $ replicate pwidth '-'
    ]

prow :: [String] -> Doc ann
prow es = ":" <> hsep [pretty $ replicate (cwidth - length e) ' ' ++ e | e <- es] <+> ":"

data Cat = Cat{posLong :: Integer, posShort :: Integer, posSpread :: Maybe Integer
  , changeLong :: Maybe Integer, changeShort :: Maybe Integer, changeSpread :: Maybe Integer
  , pctOpenLong :: Double, pctOpenShort :: Double, pctOpenSpread :: Maybe Double
  , tradersLong :: Maybe Integer, tradersShort :: Maybe Integer, tradersSpread :: Maybe Integer
  } deriving (Eq, Read, Show)

parseCOT :: String -> Either (ParseErrorBundle String Void) COT
parseCOT = parse (many parseRpt) ""

type Parser = Parsec Void String

parseRpt :: Parser Rpt
parseRpt = do
  _ <- "Disaggregated Commitments of Traders-" <* hspace
  rptType' <- ("All Futures Combined Positions" $> Futes) <|>
    ("Options and Futures Combined Positions" $> OptsAndFutes)
  date' <- " as of " *> parseDate <* hspace <* eol
  _ <- line "Reportable Positions"
  _ <- line $ some "-"
  _ <- count 3 $ someTill printChar eol
  _ <- some "-" <* eol
  contract' <- someTill printChar " - "
  exchange' <- someTill printChar "   "
  underlying' <- someTill printChar "   " <* hspace <* ":" <* eol
  code' <- "CFTC Code #" *> count 6 alphaNumChar <* hspace
  openInterest' <- "Open Interest is" *> hspace *> num <* hspace <* ":" <* eol
  (p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) <- line "Positions" *> line (row num) <* line hspace
  changesFrom' <- line $ "Changes from:" *> hspace *> parseDate
  (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10) <- line (row $ val num) <* line hspace
  _ <- line "Percent of Open Interest Represented by Each Category of Trader"
  (o0,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10) <- line (row dub) <* line hspace
  totTraders' <- line $ "Number of Traders in Each Category" *> hspace
    *> "Total Traders:" *> hspace *> num
  (t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) <- line (row $ val num) <* some "-" <* eol
  return Rpt{rptType=rptType', date=date', contract=contract'
    , exchange=exchange', underlying=underlying', code=code'
    , openInterest=openInterest', changesFrom=changesFrom', totTraders=totTraders'
    , prod=Cat{posLong=p0, posShort=p1, posSpread=Nothing, changeLong=c0
      , changeShort=c1, changeSpread=Nothing, pctOpenLong=o0, pctOpenShort=o1
      , pctOpenSpread=Nothing, tradersLong=t0, tradersShort=t1, tradersSpread=Nothing}
    , swap=Cat{posLong=p2, posShort=p3, posSpread=Just p4, changeLong=c2
      , changeShort=c3, changeSpread=c4, pctOpenLong=o2, pctOpenShort=o3
      , pctOpenSpread=Just o4, tradersLong=t2, tradersShort=t3, tradersSpread=t4}
    , managed=Cat{posLong=p5, posShort=p6, posSpread=Just p7, changeLong=c5
      , changeShort=c6, changeSpread=c7, pctOpenLong=o5, pctOpenShort=o6
      , pctOpenSpread=Just o7, tradersLong=t5, tradersShort=t6, tradersSpread=t7}
    , other=Cat{posLong=p8, posShort=p9, posSpread=Just p10, changeLong=c8
      , changeShort=c9, changeSpread=c10, pctOpenLong=o8, pctOpenShort=o9
      , pctOpenSpread=Just o10, tradersLong=t8, tradersShort=t9, tradersSpread=t10}
    }

row :: Parser a -> Parser (a,a,a,a,a,a,a,a,a,a,a)
row p = (,,,,,,,,,,) <$> p <*> p <*> p <*> p <*> p <*> p <*> p <*> p <*> p
  <*> p <*> p

parseDate :: Parser String
parseDate = do
  m <- some letterChar <* " "
  d <- some digitChar <* ", "
  y <- some digitChar
  return $ unwords [m, d ++ ",", y]

val :: Parser a -> Parser (Maybe a)
val p = fmap Just p <|> ("." *> hspace $> Nothing)

num :: Parser Integer
num = L.signed hspace $ fmap read go <* hspace
  where
    go :: Parser String
    go = fmap join $ some $ fmap pure digitChar <|> ("," $> [])

snum :: Integer -> String
snum i = case show i of
  '-':ns -> "-" <> fmt (reverse ns)
  ns -> fmt $ reverse ns
  where
    fmt s = case s of
      [] -> []
      [d] -> [d]
      [a,b] -> [b,a]
      [a,b,c] -> [c,b,a]
      a:b:c:ds -> fmt ds ++ "," ++ [c,b,a]

dub :: Parser Double
dub = L.signed hspace L.float <* hspace

sdub :: Double -> String
sdub d = showFFloat (Just 1) d ""

line :: Parser a -> Parser a
line p = ":" *> hspace *> p <* hspace <* ":" <* eol
