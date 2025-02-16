{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Symbolize.Accursed (accursedUnutterablePerformIO) where

import GHC.Exts (realWorld#)
import GHC.IO (IO (IO))

-- import GHC.Weak (Weak(..))
-- import Unsafe.Coerce (unsafeCoerce#)

-- This \"function\" has a superficial similarity to 'System.IO.Unsafe.unsafePerformIO' but
-- it is in fact a malevolent agent of chaos.
--
-- Full warning: https://hackage.haskell.org/package/bytestring-0.12.0.2/docs/Data-ByteString-Internal.html#v:accursedUnutterablePerformIO
-- (This definition is also taken from there)
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE accursedUnutterablePerformIO #-}
