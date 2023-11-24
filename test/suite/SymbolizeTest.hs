module SymbolizeTest where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Symbolize (Symbol)
import qualified Symbolize
import Data.Text (Text)
import qualified System.Mem
import Control.Monad.IO.Class (liftIO)
import qualified Data.Hashable
import qualified Debug.Trace

-- unit_simpleInternUninternTest :: IO ()
-- unit_simpleInternUninternTest = do 
--     let str = "hello" :: Text
--     let !symbol = Symbolize.intern str

--     size <- Symbolize.globalSymbolTableSize
--     size @?= 1

--     let str2 = Symbolize.unintern symbol
--     str2 @?= str


unit_globalTableStartsEmpty :: IO ()
unit_globalTableStartsEmpty = do
    size <- Symbolize.globalSymbolTableSize
    size @?= 0

hprop_symbolTableIsIdempotent = withTests 1000 $ property $ do
    -- text <- forAll $ (Gen.text (Range.linear 1 10) Gen.unicode)
    -- let list = [text | _ <- [1..100]]
    -- fmap (Symbolize.unintern . Symbolize.intern) list === list
    -- Symbolize.unintern (Symbolize.intern text) === text
    liftIO System.Mem.performGC
    texts <- forAll $ Gen.list (Range.linear 0 200) (Gen.text (Range.linear 0 20) Gen.unicode)
    let !symbols = fmap Symbolize.intern texts
    -- Debug.Trace.traceShow (fmap (show . Data.Hashable.hash) symbols)
    annotateShow (fmap Data.Hashable.hash symbols)
    let !texts2 = fmap Symbolize.unintern symbols
    liftIO System.Mem.performGC

    texts2 === texts

    
    -- fmap (Symbolize.unintern . Symbolize.intern) texts === texts
