{-# LANGUAGE OverloadedStrings #-}

module Data.COT (COT, Rpt(..), Cat(..), parseCOT) where

import Control.Monad
import Data.Functor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prettyprinter hiding (line)

type COT = [Rpt]

data Rpt = Rpt{title :: String, subtitle :: String, date :: String, repPos :: String
  , contract :: String, exchange :: String, underlying :: String
  , code :: String, openInterest :: Integer, changesFrom :: String
  , totTraders :: Integer, prod :: Cat, swap :: Cat, managed :: Cat, other :: Cat}
  deriving (Eq, Read, Show)

instance Pretty Rpt where
  pretty r@Rpt{prod=p,swap=s,managed=m,other=o} = vsep
    [ pretty (title r) <> "-" <> pretty (subtitle r) <+> "as of" <+> pretty (date r)
    , ":                                             " <> pretty (repPos r)
      <> "                                             :"
    , ":------------------------------------------------------------------------------------------------------------- :"
    , ": Producer/Merchant :                             :                             :                              :"
    , ":  Processor/User   :        Swap Dealers         :        Managed Money        :      Other Reportables       :"
    , ":   Long  :  Short  :   Long  :  Short  :Spreading:   Long  :  Short  :Spreading:   Long  :  Short  :Spreading :"
    , "----------------------------------------------------------------------------------------------------------------"
    , pretty (contract r) <+> "-" <+> pretty (exchange r) <> "   " <> "(" <> pretty (underlying r) <> ")" <+> ":"
    , "CFTC Code #" <> pretty (code r) <+> "Open Interest is" <+> pretty (openInterest r)
    , ": Positions                                                                                                    :"
    , ":" <+> pretty (posLong p) <+> pretty (posShort p) <+> pretty (posLong s)
        <+> pretty (posShort s) <+> maybe "." pretty (posSpread s)
        <+> pretty (posLong m) <+> pretty (posShort m)
        <+> maybe "." pretty (posSpread m) <+> pretty (posLong o)
        <+> pretty (posShort o) <+> maybe "." pretty (posSpread o) <+> ":"
    , ":                                                                                                              :"
    , ": Changes from:" <+> pretty (changesFrom r) <+> ":"
    , ":" <+> pretty (changeLong p) <+> pretty (changeShort p)
        <+> pretty (changeLong s) <+> pretty (changeShort s)
        <+> maybe "." pretty (changeSpread s) <+> pretty (changeLong m)
        <+> pretty (changeShort m) <+> maybe "." pretty (changeSpread m)
        <+> pretty (changeLong o) <+> pretty (changeShort o)
        <+> maybe "." pretty (changeSpread o) <+> ":"
    ]

data Cat = Cat{posLong :: Integer, posShort :: Integer, posSpread :: Maybe Integer
  , changeLong :: Integer, changeShort :: Integer, changeSpread :: Maybe Integer
  , pctOpenLong :: Double, pctOpenShort :: Double, pctOpenSpread :: Maybe Double
  , tradersLong :: Integer, tradersShort :: Integer, tradersSpread :: Maybe Integer
  } deriving (Eq, Read, Show)

parseCOT :: String -> Either (ParseErrorBundle String Void) COT
parseCOT = parse (many parseRpt) ""

type Parser = Parsec Void String

parseRpt :: Parser Rpt
parseRpt = do
  title' <- unwords <$> someTill word "-"
  subtitle' <- unwords <$> someTill word "as of "
  date' <- parseDate <* hspace <* eol
  repPos' <- line $ some word
  _ <- line $ some "-"
  _ <- count 3 $ someTill printChar eol
  _ <- some "-" <* eol
  contract' <- someTill word "- "
  exchange' <- someTill word "("
  underlying' <- someTill printChar ")" <* hspace <* ":" <* eol
  code' <- "CFTC Code #" *> some digitChar <* hspace
  openInterest' <- "Open Interest is" *> hspace *> num <* hspace <* ":" <* eol
  ps <- line "Positions" *> line (some num) <* line hspace
  changesFrom' <- line $ "Changes from:" *> hspace *> parseDate
  cs <- line (some num) <* line hspace
  _ <- line "Percent of Open Interest Represented by Each Category of Trader"
  oi <- line (some dub) <* line hspace
  totTraders' <- line $ "Number of Traders in Each Category" *> hspace
    *> "Total Traders:" *> hspace *> num
  ts <- line (some num) <* some "-" <* eol
  case mkCats ps cs oi ts of
    Left err -> error err
    Right (prod',swap',managed',other') -> return Rpt{title=title'
      , subtitle=subtitle', date=date', repPos=unwords repPos'
      , contract=unwords contract', exchange=unwords exchange'
      , underlying=underlying', code=code', openInterest=openInterest'
      , changesFrom=changesFrom', totTraders=totTraders', prod=prod', swap=swap'
      , managed=managed', other=other'}

mkCats :: [Integer] -> [Integer] -> [Double] -> [Integer] -> Either String (Cat, Cat, Cat, Cat)
mkCats [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10] [c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10]
  [o0,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10] [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10]
  = Right (prod', swap', managed', other')
  where
    prod'=Cat{posLong=p0, posShort=p1, posSpread=Nothing, changeLong=c0
      , changeShort=c1, changeSpread=Nothing, pctOpenLong=o0, pctOpenShort=o1
      , pctOpenSpread=Nothing, tradersLong=t0, tradersShort=t1
      , tradersSpread=Nothing}
    swap'=Cat{posLong=p2, posShort=p3, posSpread=Just p4, changeLong=c2
      , changeShort=c3, changeSpread=Just c4, pctOpenLong=o2, pctOpenShort=o3
      , pctOpenSpread=Just o4, tradersLong=t2, tradersShort=t3
      , tradersSpread=Just t4}
    managed'=Cat{posLong=p5, posShort=p6, posSpread=Just p7, changeLong=c5
      , changeShort=c6, changeSpread=Just c7, pctOpenLong=o5, pctOpenShort=o6
      , pctOpenSpread=Just o7, tradersLong=t5, tradersShort=t6
      , tradersSpread=Just t7}
    other'=Cat{posLong=p8, posShort=p9, posSpread=Just p10, changeLong=c8
      , changeShort=c9, changeSpread=Just c10, pctOpenLong=o8, pctOpenShort=o9
      , pctOpenSpread=Just o10, tradersLong=t8, tradersShort=t9
      , tradersSpread=Just t10}
mkCats _ _ _ _ = Left "Invalid category data"

parseDate :: Parser String
parseDate = do
  m <- some letterChar <* " "
  d <- some digitChar <* ", "
  y <- some digitChar
  return $ unwords [m, d ++ ",", y]

word :: Parser String
word = some alphaNumChar <* hspace

num :: Parser Integer
num = L.signed hspace $ fmap read go <* hspace
  where
    go :: Parser String
    go = fmap join $ some $ fmap pure digitChar <|> ("," $> [])

dub :: Parser Double
dub = L.signed hspace L.float <* hspace

line :: Parser a -> Parser a
line p = ":" *> hspace *> p <* hspace <* ":" <* eol
