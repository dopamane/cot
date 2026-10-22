module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.COT
import Paths_cot
import Prettyprinter
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Text.Megaparsec

main :: IO ()
main = getDataDir >>= \dataDir ->
  defaultMain $ testGroup "Test.COT"
  [ trip "rbob" $ dataDir </> "rbob.txt"
  , trip "wti" $ dataDir </> "wti.txt"
  , trip "fuel-oil" $ dataDir </> "fuel-oil.txt"
  , trip "petroleum" $ dataDir </> "petroleum.txt"
  , trip "agriculture" $ dataDir </> "agriculture.txt"
  ]

trip :: String -> FilePath -> TestTree
trip testName dataFile = goldenVsString testName dataFile $ do
  result <- parseCOT <$> readFile dataFile
  case result of
    Left err -> error $ errorBundlePretty err
    Right cot -> return $ BS.pack $ show (vsep $ map pretty cot) ++ "\n"
