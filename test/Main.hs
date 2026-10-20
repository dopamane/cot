module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.COT
import Paths_cot
import Prettyprinter
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = getDataDir >>= \dataDir ->
  defaultMain $ testGroup "Test"
  [ goldenVsString "rbob" (dataDir </> "rbob.txt") $ do
      s <- readFile $ dataDir </> "rbob.txt"
      case parseCOT s of
        Left _ -> error "parse fail"
        Right cot -> return $ BS.pack $ show (vsep $ map pretty cot) ++ "\n"
  ]
