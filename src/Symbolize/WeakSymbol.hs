{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
module Symbolize.WeakSymbol (WeakSymbol, new, deref) where

import Data.Array.Byte (ByteArray (ByteArray))
import GHC.Exts (ByteArray#, StableName#, Weak#, deRefWeak#, mkWeak#, makeStableName#)
import GHC.IO (IO (IO))
import Symbolize.Accursed qualified as Accursed

-- Inside the WeakSymbol
-- we keep:
-- - A weak pointer to the underlying ByteArray#.
--   This weak pointer will be invalidated (turn into a 'tombstone')
--   when the final instance of this symbol is GC'd
-- - A `StableName` for the same ByteArray#
--   This ensures we have a stable hash even in the presence of the ByteArray#
--   being moved around by the GC.
--   We never read it after construction,
--   but by keeping it around until the WeakSymbol is removed by the finalizer,
--   we ensure that future calls to `makeStableName` return the same hash.
data WeakSymbol where
  WeakSymbol# :: Weak# ByteArray# -> StableName# ByteArray# -> WeakSymbol

-- | Create a new weak symbol
-- based on the given symbol content ByteArray
-- and finalizer to run when the weak symbol
-- is no longer needed.
new :: ByteArray# -> IO () -> IO WeakSymbol
{-# INLINE new #-}
new ba# (IO finalizer#) =
    -- SAFETY: This should even be safe
    -- in the presence of inlining, CSE and full laziness
    --
    -- because the result is outwardly pure
    -- and the finalizer we use is idempotent
  IO $ \s1 -> case mkWeak# ba# ba# finalizer# s1 of
    (# s2, weak# #) ->
      case makeStableName# ba# s2 of
        (# s3, sname# #) ->
          (# s3, WeakSymbol# weak# sname# #)

-- | Attempt to get back the containing ByteArray#
-- by looking inside this `WeakSymbol`
--
-- Returns `Nothing` if it was GC'd in the meantime
-- (which may be before, after or concurrently with when the finalizer runs)
deref :: WeakSymbol -> Maybe ByteArray
{-# INLINE deref #-}
deref (WeakSymbol# w _sn) =
    -- SAFETY: This should even be safe
    -- in the presence of inlining, CSE and full laziness;
    Accursed.accursedUnutterablePerformIO $ IO $ \s ->
  case deRefWeak# w s of
    (# s1, flag, p #) -> case flag of
      0# -> (# s1, Nothing #)
      _ -> (# s1, Just (ByteArray p) #)
