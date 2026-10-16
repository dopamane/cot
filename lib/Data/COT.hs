module Data.COT where

data COT = COT{title :: String, date :: String, subtitle :: String
  , contract :: String, exchange :: String, underlying :: String
  , code :: String, openInterest :: Integer, changesFrom :: String
  , totTraders :: Integer, prod :: Cat, swap :: Cat, managed :: Cat, other :: Cat}
  deriving (Eq, Read, Show)

data Cat = Cat{posLong :: Integer, posShort :: Integer, posSpread :: Maybe Integer
  , changeLong :: Integer, changeShort :: Integer, changeSpread :: Maybe Integer
  , pctOpenLong :: Double, pctOpenShort :: Double, pctOpenSpread :: Maybe Double
  , tradersLong :: Integer, tradersShort :: Integer, tradersSpread :: Maybe Integer
  } deriving (Eq, Read, Show)
