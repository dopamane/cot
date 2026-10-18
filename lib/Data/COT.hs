{-# LANGUAGE OverloadedStrings #-}

module Data.COT (COT, Rpt(..), Cat(..), parseCOT, prettyCOT) where

import Control.Monad
import Data.Functor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type COT = [Rpt]

data Rpt = Rpt{title :: String, subtitle :: String, date :: String, repPos :: String
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
  return Rpt{title=title', subtitle=subtitle', date=date', repPos=unwords repPos'
    , contract=unwords contract', exchange=unwords exchange'
    , underlying=underlying', code=code', openInterest=openInterest'
    , changesFrom=changesFrom', totTraders=totTraders', prod=prod', swap=swap'
    , managed=managed', other=other'}
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

num :: Parser Integer
num = L.signed hspace $ fmap read go <* hspace
  where
    go :: Parser String
    go = fmap join $ some $ fmap pure digitChar <|> ("," $> [])

dub :: Parser Double
dub = L.signed hspace L.float <* hspace

line :: Parser a -> Parser a
line p = ":" *> hspace *> p <* hspace <* ":" <* eol

prettyCOT :: COT -> String
prettyCOT _ = ""
