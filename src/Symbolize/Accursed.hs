{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Symbolize.Accursed (accursedUnutterablePerformIO) where

import GHC.IO (IO(IO))
import GHC.Base (realWorld#)

-- This \"function\" has a superficial similarity to 'System.IO.Unsafe.unsafePerformIO' but
-- it is in fact a malevolent agent of chaos.
--
-- Full warning: https://hackage.haskell.org/package/bytestring-0.12.0.2/docs/Data-ByteString-Internal.html#v:accursedUnutterablePerformIO
-- (This definition is also taken from there)
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r
{-# INLINE accursedUnutterablePerformIO #-}