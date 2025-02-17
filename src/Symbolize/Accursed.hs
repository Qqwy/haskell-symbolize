{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide, prune #-}
module Symbolize.Accursed (accursedUnutterablePerformIO, utf8CompareByteArray#, shortTextFromBA, byteArrayStableNameHash##) where

import GHC.Exts (realWorld#, ByteArray#, sizeofByteArray#, isTrue#, (>=#), andI#, word8ToWord#, indexWord8Array#, (+#), gtWord#, ltWord#, makeStableName#, stableNameToInt#, Int#)
import GHC.IO (IO (IO))
import Data.Array.Byte (ByteArray(ByteArray))
import Data.Text.Short (ShortText)
import Data.ByteString.Short (ShortByteString(SBS))
import qualified Data.Text.Short.Unsafe as Text.Short.Unsafe

-- This \"function\" has a superficial similarity to 'System.IO.Unsafe.unsafePerformIO' but
-- it is in fact a malevolent agent of chaos.
--
-- Full warning: https://hackage.haskell.org/package/bytestring-0.12.0.2/docs/Data-ByteString-Internal.html#v:accursedUnutterablePerformIO
-- (This definition is also taken from there)
accursedUnutterablePerformIO :: IO a -> a
{-# INLINE accursedUnutterablePerformIO #-}
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

-- Lifted from `base`'s internal `GHC.Encoding.UTF8` module.
-- Since that module could change in any minor version bump,
-- the code is copied to here.
--
-- This special comparison is necessary since
-- normal comparison of `ByteArray`s is non-lexicographic.
utf8CompareByteArray# :: ByteArray# -> ByteArray# -> Ordering
{-# INLINE utf8CompareByteArray# #-}
utf8CompareByteArray# a1 a2 = go 0# 0#
   -- UTF-8 has the property that sorting by bytes values also sorts by
   -- code-points.
   -- BUT we use "Modified UTF-8" which encodes \0 as 0xC080 so this property
   -- doesn't hold and we must explicitly check this case here.
   -- Note that decoding every code point would also work but it would be much
   -- more costly.
   where
       !sz1 = sizeofByteArray# a1
       !sz2 = sizeofByteArray# a2
       go off1 off2
         | isTrue# ((off1 >=# sz1) `andI#` (off2 >=# sz2)) = EQ
         | isTrue# (off1 >=# sz1)                          = LT
         | isTrue# (off2 >=# sz2)                          = GT
         | otherwise =
               let !b1_1 = word8ToWord# (indexWord8Array# a1 off1)
                   !b2_1 = word8ToWord# (indexWord8Array# a2 off2)
               in case b1_1 of
                  0xC0## -> case b2_1 of
                     0xC0## -> go (off1 +# 1#) (off2 +# 1#)
                     _      -> case word8ToWord# (indexWord8Array# a1 (off1 +# 1#)) of
                        0x80## -> LT
                        _      -> go (off1 +# 1#) (off2 +# 1#)
                  _      -> case b2_1 of
                     0xC0## -> case word8ToWord# (indexWord8Array# a2 (off2 +# 1#)) of
                        0x80## -> GT
                        _      -> go (off1 +# 1#) (off2 +# 1#)
                     _   | isTrue# (b1_1 `gtWord#` b2_1) -> GT
                         | isTrue# (b1_1 `ltWord#` b2_1) -> LT
                         | otherwise                     -> go (off1 +# 1#) (off2 +# 1#)

-- Helper function to go from ByteArray to ShortText.
-- Does *not* check whether it is valid UTF-8!
shortTextFromBA :: ByteArray -> ShortText
{-# INLINE shortTextFromBA #-}
shortTextFromBA (ByteArray ba#) = Text.Short.Unsafe.fromShortByteStringUnsafe (SBS ba#)

-- Calculate the stable name for an unlifted ByteArray#,
-- and immediately calculate its hash
byteArrayStableNameHash## :: ByteArray# -> Int#
{-# INLINE byteArrayStableNameHash## #-}
byteArrayStableNameHash##  ba# =
    case makeStableName# ba# realWorld# of
        (# _, sname# #) -> stableNameToInt# sname#
