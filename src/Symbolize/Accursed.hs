{-# LANGUAGE GHC2021, MagicHash, UnboxedTuples #-}
module Symbolize.Accursed (accursedUnutterablePerformIO, sameByteArray, ensurePinned) where

import GHC.IO (IO(IO))
import Data.Primitive.ByteArray (ByteArray, ByteArray#)
import Data.Primitive.ByteArray qualified as ByteArray
import GHC.Exts (realWorld#, reallyUnsafePtrEquality, isTrue#)
import Unsafe.Coerce (unsafeCoerce#)

-- This \"function\" has a superficial similarity to 'System.IO.Unsafe.unsafePerformIO' but
-- it is in fact a malevolent agent of chaos.
--
-- Full warning: https://hackage.haskell.org/package/bytestring-0.12.0.2/docs/Data-ByteString-Internal.html#v:accursedUnutterablePerformIO
-- (This definition is also taken from there)
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r
{-# INLINE accursedUnutterablePerformIO #-}


-- | Do two byte arrays share the same pointer?
sameByteArray :: ByteArray# -> ByteArray# -> Bool
sameByteArray ba1 ba2 =
    case reallyUnsafePtrEquality (unsafeCoerce# ba1 :: ()) (unsafeCoerce# ba2 :: ()) of
      r -> isTrue# r

-- | If already pinned, returns the input unchanged.
-- Otherwise, creates a pinned copy
ensurePinned :: ByteArray -> ByteArray
ensurePinned ba 
  | ByteArray.isByteArrayPinned ba = ba
  | otherwise = accursedUnutterablePerformIO $ do
      pinned <- ByteArray.newPinnedByteArray (ByteArray.sizeofByteArray ba)
      ByteArray.copyByteArray pinned 0 ba 0 (ByteArray.sizeofByteArray ba)
      ByteArray.unsafeFreezeByteArray pinned

