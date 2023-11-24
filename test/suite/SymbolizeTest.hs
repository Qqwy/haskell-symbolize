module SymbolizeTest where

-- import qualified System.Mem

import qualified Control.Concurrent.Async
import Control.Monad.IO.Class (liftIO)
import qualified Data.Hashable
import Data.Text (Text)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Symbolize
import qualified System.Mem
import Test.Tasty.HUnit

unit_simpleInternUninternTest :: IO ()
unit_simpleInternUninternTest = do
  System.Mem.performGC

  let str = "hello" :: Text
  let !symbol = Symbolize.intern str

  size <- Symbolize.globalSymbolTableSize
  size @?= 1

  let str2 = Symbolize.unintern symbol
  str2 @?= str

hprop_symbolTableIsIdempotent :: Property
hprop_symbolTableIsIdempotent = withTests 1000 $ property $ do
  texts <- forAll $ Gen.list (Range.linear 0 200) (Gen.text (Range.linear 0 20) Gen.unicode)
  let !symbols = fmap Symbolize.intern texts
  annotateShow (fmap Data.Hashable.hash symbols)
  let !texts2 = fmap Symbolize.unintern symbols

  texts2 === texts

hprop_concurrentAccessDoesNotCorruptTable :: Property
hprop_concurrentAccessDoesNotCorruptTable = withTests 500 $ property $ do
  let numCores = 8
  texts <- forAll $ Gen.list (Range.linear 0 200) (Gen.text (Range.linear 0 20) Gen.unicode)
  results <- liftIO $ Control.Concurrent.Async.forConcurrently [(1 :: Integer) .. numCores] $ \_ -> do
    let !texts2 = fmap (\val -> Symbolize.unintern $! Symbolize.intern $! val) texts
    pure texts2

  mapM_ (=== texts) results
