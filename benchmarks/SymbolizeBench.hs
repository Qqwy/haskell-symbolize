{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedLists #-}
import Test.Tasty.Bench

import Symbolize qualified

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as V

mkTexts :: IO (Vector Text)
mkTexts = do
    let vec = Text.pack . show @Int <$> [0..3200]
    pure vec

roundtripMany :: Vector Text -> Vector Text
roundtripMany = fmap (\val -> Symbolize.unintern $! Symbolize.intern $! val)

main :: IO ()
main = defaultMain
  [ env mkTexts $ \texts ->
    bgroup "intern/unintern roundtrip, no duplicates"
    [ bench "10"  $ nf roundtripMany $! V.take 10 texts
    , bench "100" $ nf roundtripMany $! V.take 100 texts
    , bench "200" $ nf roundtripMany $! V.take 200 texts
    , bench "400" $ nf roundtripMany $! V.take 400 texts
    , bench "800" $ nf roundtripMany $! V.take 800 texts
    , bench "1600" $ nf roundtripMany $! V.take 1600 texts
    , bench "3200" $ nf roundtripMany $! V.take 3200 texts
    ]
  ]
