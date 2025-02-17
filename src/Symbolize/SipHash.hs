-- Source code taken from the `memory` library
-- https://hackage.haskell.org/package/memory
--
-- But adapted to work on (not necessarily pinned) `ByteArray`
-- rather than on `Ptr`s.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module Symbolize.SipHash
    ( SipKey(..)
    , SipHash(..)
    , hash
    , hashWith
    ) where

import Data.Word ( Word8, Word32, Word64, byteSwap64 )
import Data.Bits ( Bits(xor, unsafeShiftL, (.|.), rotateL) )
import           Data.Typeable (Typeable)
import Control.Monad ( liftM2, liftM3, liftM4, liftM5 )
import Foreign.Ptr ( Ptr, castPtr, )
import Foreign.Storable ( Storable(peek, poke) )
import Foreign (alloca)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Array.Byte (ByteArray (ByteArray))
import GHC.Int (Int(I#))
import GHC.Exts (indexWord8Array#, indexWord8ArrayAsWord64#, sizeofByteArray#)
import GHC.Word (Word8(..), Word64(..))
import Data.Functor.Identity (Identity(runIdentity))
import System.Random (Uniform)
import System.Random.Stateful (Uniform(uniformM))

-- | SigHash Key
data SipKey = SipKey {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving Show

instance Uniform SipKey where
    uniformM g = do
        a <- uniformM g
        b <- uniformM g
        pure (SipKey a b)

-- | Siphash tag value
newtype SipHash = SipHash Word64
    deriving (Show,Eq,Ord,Typeable)

data InternalState = InternalState {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

-- | produce a siphash with a key and a memory pointer + length.
hash :: SipKey -> ByteArray -> SipHash
{-# INLINE hash #-}
hash = hashWith 2 4


-- | same as 'hash', except also specifies the number of sipround iterations for compression and digest.
hashWith :: Int       -- ^ siphash C
         -> Int       -- ^ siphash D
         -> SipKey    -- ^ key for the hash
         -> ByteArray -- ^ start of byte array
         -> SipHash
{-# INLINE hashWith #-}
hashWith c d key startPtr = runIdentity $ runHash (initSip key) startPtr 0 totalLen
  where
        !totalLen = sizeofByteArray startPtr
        runHash !st !ba !ptr l
            | l > 7     =
                let v = indexWord8ArrayAsWord64 ba ptr in runHash (process st (fromLE (LE v))) ba (ptr + 8) (l-8)
            | otherwise = do
                let peekByteOff pointer offset = pure (indexByteArray ba (pointer + offset))
                let !lengthBlock = (fromIntegral totalLen `mod` 256) `unsafeShiftL` 56
                (finish . process st) `fmap` case l of
                    0 -> do return lengthBlock
                    1 -> do v0 <- peekByteOff ptr 0
                            return (lengthBlock .|. to64 v0)
                    2 -> do (v0,v1) <- liftM2 (,) (peekByteOff ptr 0) (peekByteOff ptr 1)
                            return (lengthBlock
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    3 -> do (v0,v1,v2) <- liftM3 (,,) (peekByteOff ptr 0) (peekByteOff ptr 1) (peekByteOff ptr 2)
                            return (    lengthBlock
                                    .|. (to64 v2 `unsafeShiftL` 16)
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    4 -> do (v0,v1,v2,v3) <- liftM4 (,,,) (peekByteOff ptr 0) (peekByteOff ptr 1) (peekByteOff ptr 2)
                                                          (peekByteOff ptr 3)
                            return (    lengthBlock
                                    .|. (to64 v3 `unsafeShiftL` 24)
                                    .|. (to64 v2 `unsafeShiftL` 16)
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    5 -> do (v0,v1,v2,v3,v4) <- liftM5 (,,,,) (peekByteOff ptr 0) (peekByteOff ptr 1) (peekByteOff ptr 2)
                                                              (peekByteOff ptr 3) (peekByteOff ptr 4)
                            return (    lengthBlock
                                    .|. (to64 v4 `unsafeShiftL` 32)
                                    .|. (to64 v3 `unsafeShiftL` 24)
                                    .|. (to64 v2 `unsafeShiftL` 16)
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    6 -> do v0 <- peekByteOff ptr 0
                            v1 <- peekByteOff ptr 1
                            v2 <- peekByteOff ptr 2
                            v3 <- peekByteOff ptr 3
                            v4 <- peekByteOff ptr 4
                            v5 <- peekByteOff ptr 5
                            return (    lengthBlock
                                    .|. (to64 v5 `unsafeShiftL` 40)
                                    .|. (to64 v4 `unsafeShiftL` 32)
                                    .|. (to64 v3 `unsafeShiftL` 24)
                                    .|. (to64 v2 `unsafeShiftL` 16)
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    7 -> do v0 <- peekByteOff ptr 0
                            v1 <- peekByteOff ptr 1
                            v2 <- peekByteOff ptr 2
                            v3 <- peekByteOff ptr 3
                            v4 <- peekByteOff ptr 4
                            v5 <- peekByteOff ptr 5
                            v6 <- peekByteOff ptr 6
                            return (    lengthBlock
                                    .|. (to64 v6 `unsafeShiftL` 48)
                                    .|. (to64 v5 `unsafeShiftL` 40)
                                    .|. (to64 v4 `unsafeShiftL` 32)
                                    .|. (to64 v3 `unsafeShiftL` 24)
                                    .|. (to64 v2 `unsafeShiftL` 16)
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    _ -> error "siphash: internal error: cannot happens"

        {-# INLINE to64 #-}
        to64 :: Word8 -> Word64
        to64 = fromIntegral

        {-# INLINE process #-}
        process istate m = newState
            where newState = postInject $! runRoundsCompression $! preInject istate
                  preInject  (InternalState v0 v1 v2 v3) = InternalState v0 v1 v2 (v3 `xor` m)
                  postInject (InternalState v0 v1 v2 v3) = InternalState (v0 `xor` m) v1 v2 v3

        {-# INLINE finish #-}
        finish istate = getDigest $! runRoundsDigest $! preInject istate
            where getDigest (InternalState v0 v1 v2 v3) = SipHash (v0 `xor` v1 `xor` v2 `xor` v3)
                  preInject (InternalState v0 v1 v2 v3) = InternalState v0 v1 (v2 `xor` 0xff) v3

        {-# INLINE doRound #-}
        doRound (InternalState v0 v1 v2 v3) =
              let !v0'    = v0 + v1
                  !v2'    = v2 + v3
                  !v1'    = v1 `rotateL` 13
                  !v3'    = v3 `rotateL` 16
                  !v1''   = v1' `xor` v0'
                  !v3''   = v3' `xor` v2'
                  !v0''   = v0' `rotateL` 32
                  !v2''   = v2' + v1''
                  !v0'''  = v0'' + v3''
                  !v1'''  = v1'' `rotateL` 17
                  !v3'''  = v3'' `rotateL` 21
                  !v1'''' = v1''' `xor` v2''
                  !v3'''' = v3''' `xor` v0'''
                  !v2'''  = v2'' `rotateL` 32
               in InternalState v0''' v1'''' v2''' v3''''

        {-# INLINE runRoundsCompression #-}
        runRoundsCompression st
            | c == 2    = doRound $! doRound st
            | otherwise = loopRounds c st

        {-# INLINE runRoundsDigest #-}
        runRoundsDigest st
            | d == 4    = doRound $! doRound $! doRound $! doRound st
            | otherwise = loopRounds d st

        {-# INLINE loopRounds #-}
        loopRounds 1 !v = doRound v
        loopRounds n !v = loopRounds (n-1) (doRound v)

        {-# INLINE initSip #-}
        initSip (SipKey k0 k1) = InternalState (k0 `xor` 0x736f6d6570736575)
                                               (k1 `xor` 0x646f72616e646f6d)
                                               (k0 `xor` 0x6c7967656e657261)
                                               (k1 `xor` 0x7465646279746573)

-- | Convert from a little endian value to the cpu endianness
fromLE :: LE Word64 -> Word64
#ifdef ARCH_IS_LITTLE_ENDIAN
fromLE (LE a) = a
#elif ARCH_IS_BIG_ENDIAN
fromLE (LE a) = byteSwap64 a
#else
fromLE (LE a) = if getSystemEndianness == LittleEndian then a else byteSwap64 a
#endif
{-# INLINE fromLE #-}


-- | Little Endian value
newtype LE a = LE { unLE :: a }
    deriving (Show, Eq)


-- | represent the CPU endianness
--
-- Big endian system stores bytes with the MSB as the first byte.
-- Little endian system stores bytes with the LSB as the first byte.
--
-- middle endian is purposely avoided.
data Endianness = LittleEndian
                | BigEndian
                deriving (Show,Eq)

-- | Return the system endianness
getSystemEndianness :: Endianness
#ifdef ARCH_IS_LITTLE_ENDIAN
getSystemEndianness = LittleEndian
#elif ARCH_IS_BIG_ENDIAN
getSystemEndianness = BigEndian
#else
getSystemEndianness
    | isLittleEndian = LittleEndian
    | isBigEndian    = BigEndian
    | otherwise      = error "cannot determine endianness"
  where
        isLittleEndian = endianCheck == 2
        isBigEndian    = endianCheck == 1
        endianCheck    = unsafeDupablePerformIO $ alloca $ \p -> do
                            poke p (0x01000002 :: Word32)
                            peek (castPtr p :: Ptr Word8)
#endif

indexByteArray :: ByteArray -> Int -> Word8
{-# INLINE indexByteArray #-}
indexByteArray (ByteArray arr#) (I# i#) = W8# (indexWord8Array# arr# i#)

indexWord8ArrayAsWord64 :: ByteArray -> Int -> Word64
{-# INLINE indexWord8ArrayAsWord64 #-}
indexWord8ArrayAsWord64 (ByteArray arr#) (I# i#) = W64# (indexWord8ArrayAsWord64# arr# i#)

-- | Size of the byte array in bytes.
sizeofByteArray :: ByteArray -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray (ByteArray arr#) = I# (sizeofByteArray# arr#)
