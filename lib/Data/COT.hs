{-# LANGUAGE OverloadedStrings #-}

module Data.COT (COT, Rpt(..), Cat(..), parseCOT, prettyCOT) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type COT = [Rpt]

data Rpt = Rpt{title :: String, date :: String, subtitle :: String
  , contract :: String, exchange :: String, underlying :: String
  , code :: String, openInterest :: Integer, changesFrom :: String
  , totTraders :: Integer, prod :: Cat, swap :: Cat, managed :: Cat, other :: Cat}
  deriving (Eq, Read, Show)

data Cat = Cat{posLong :: Integer, posShort :: Integer, posSpread :: Maybe Integer
  , changeLong :: Integer, changeShort :: Integer, changeSpread :: Maybe Integer
  , pctOpenLong :: Double, pctOpenShort :: Double, pctOpenSpread :: Maybe Double
  , tradersLong :: Integer, tradersShort :: Integer, tradersSpread :: Maybe Integer
  } deriving (Eq, Read, Show)

emptyCat :: Cat
emptyCat = Cat 0 0 Nothing 0 0 Nothing 0 0 Nothing 0 0 Nothing

parseCOT :: String -> Either (ParseErrorBundle String Void) COT
parseCOT = parse (some parseRpt) ""

type Parser = Parsec Void String

parseRpt :: Parser Rpt
parseRpt = do
  title' <- unwords <$> some word <* "-" <* someTill word "as of "
  date' <- parseDate <* hspace <* eol
  return Rpt{title=title', date=date', subtitle="", contract="", exchange=""
    , underlying="", code="", openInterest=0, changesFrom="", totTraders=0
    , prod=prod', swap=swap', managed=managed', other=other'}
  where
    prod'=emptyCat
    swap'=emptyCat
    managed'=emptyCat
    other'=emptyCat

parseDate :: Parser String
parseDate = do
  m <- some letterChar <* " "
  d <- some digitChar <* ", "
  y <- some digitChar
  return $ unwords [m, d ++ ",", y]

word :: Parser String
word = some alphaNumChar <* hspace

prettyCOT :: COT -> String
prettyCOT _ = ""
