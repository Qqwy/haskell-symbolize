{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide, prune #-}

module Symbolize.SymbolTable
  ( insertGlobal,
    lookupGlobal,
    removeGlobal,
    GlobalSymbolTable,
    globalSymbolTable,
    globalSymbolTableSize,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Array.Byte (ByteArray (ByteArray))
import Data.Foldable qualified as Foldable
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import GHC.Exts (ByteArray#, StableName#, Weak#, deRefWeak#, makeStableName#, mkWeak#)
import GHC.IO (IO (IO), unsafePerformIO)
import Symbolize.Accursed qualified
import Symbolize.Accursed qualified as Accursed
import Symbolize.SipHash qualified as SipHash
import System.IO.Unsafe qualified
import System.Random.Stateful qualified as Random (Uniform (uniformM), globalStdGen)
import Prelude hiding (lookup)

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

newtype SymbolTable = SymbolTable (IntMap (NonEmpty WeakSymbol))

-- | The global Symbol Table, containing a mapping between each symbol's textual representation and its deduplicated pointer.
--
-- You cannot manipulate the table itself directly,
-- but you can use `globalSymbolTable` to get a handle to it and use its `Show` instance for introspection.
--
-- `globalSymbolTableSize` can similarly be used to get the current size of the table.
--
-- Current implementation details (these might change even between PVP-compatible versions):
--
-- - An `IntMap` is used for mapping $(SipHash text) -> weak symbol$.
--   Such an IntMap has O(min(n, 64)) lookup time.
-- - Since SipHash is used as hashing algorithm and the key that is used
--   is randomized on global table initialization,
--   the table is resistent to HashDoS attacks.
data GlobalSymbolTable = GlobalSymbolTable (IORef SymbolTable) SipHash.SipKey

newtype Hash = Hash {hashToInt :: Int}

-- | What exactly this `Show` instance prints might change between PVP-compatible versions
instance Show GlobalSymbolTable where
  -- SAFETY: We're only reading, and do not care about performance here.
  show (GlobalSymbolTable table _) = System.IO.Unsafe.unsafePerformIO $ do
    SymbolTable symtab <- IORef.readIORef table
    let contents = Data.List.sort $ map Accursed.shortTextFromBA $ foldMap aliveWeaks $ IntMap.elems symtab
    pure $ "GlobalSymbolTable { size = " <> show (length contents) <> ", symbols = " <> show contents <> " }"

insertGlobal :: ByteArray# -> IO ByteArray
{-# INLINE insertGlobal #-}
insertGlobal ba# = do
  GlobalSymbolTable gsymtab sipkey <- globalSymbolTable
  let !key = calculateHash sipkey ba#
  -- SAFETY: If the table IORef contested, 
  -- this might trigger `weak` creation for the same bytestring from multiple threads
  -- at the same time.
  -- But finalization is idempotent, and only when a thread finally wins the Compare-and-Swap
  -- will its `weak` pointer be inserted (or alternatively another previously-inserted `ba` returned).
  -- So once this function returns, we can be sure we've returned a deduplicated ByteArray
  !weak <- mkWeakSymbol ba# (removeGlobal key)
  IORef.atomicModifyIORef' gsymtab $ \table ->
    case lookup ba# sipkey table of
      Just ba -> (table, ba)
      Nothing ->
        (insert key weak table, ByteArray ba#)

lookupGlobal :: ByteArray# -> IO (Maybe ByteArray)
{-# INLINE lookupGlobal #-}
lookupGlobal ba# = do
  GlobalSymbolTable gsymtab sipkey <- globalSymbolTable
  table <- IORef.readIORef gsymtab
  pure (lookup ba# sipkey table)

removeGlobal :: Hash -> IO ()
{-# INLINE removeGlobal #-}
removeGlobal !key = do
  GlobalSymbolTable gsymtab _ <- globalSymbolTable
  IORef.atomicModifyIORef' gsymtab $ \table ->
    (remove key table, ())

insert :: Hash -> WeakSymbol -> SymbolTable -> SymbolTable
{-# INLINE insert #-}
insert key weak (SymbolTable table) =
  let table' = IntMap.alter (Just . maybe (pure weak) (NonEmpty.cons weak)) (hashToInt key) table
   in SymbolTable table'

lookup :: ByteArray# -> SipHash.SipKey -> SymbolTable -> Maybe ByteArray
{-# INLINE lookup #-}
lookup ba# sipkey (SymbolTable table) = do
  let !key = calculateHash sipkey ba#
  weaks <- IntMap.lookup (hashToInt key) table
  Foldable.find (\other -> other == ByteArray ba#) (aliveWeaks weaks)

remove :: Hash -> SymbolTable -> SymbolTable
{-# INLINE remove #-}
remove (Hash key) (SymbolTable table) =
  let table' = IntMap.update removeTombstones key table
   in SymbolTable table'
  where
    removeTombstones = NonEmpty.nonEmpty . NonEmpty.filter isNoTombstone
    isNoTombstone weak =
      case deRefWeakSymbol weak of
        Nothing -> False
        Just _ -> True

calculateHash :: SipHash.SipKey -> ByteArray# -> Hash
{-# INLINE calculateHash #-}
calculateHash sipkey ba# =
  let (SipHash.SipHash word) = SipHash.hash sipkey (ByteArray ba#)
   in Hash (fromIntegral word)

mkWeakSymbol :: ByteArray# -> IO () -> IO WeakSymbol
{-# INLINE mkWeakSymbol #-}
mkWeakSymbol ba# (IO finalizer#) = 
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

deRefWeakSymbol :: WeakSymbol -> Maybe ByteArray
{-# INLINE deRefWeakSymbol #-}
deRefWeakSymbol (WeakSymbol# w _sn) = 
    -- SAFETY: This should even be safe
    -- in the presence of inlining, CSE and full laziness;
    Symbolize.Accursed.accursedUnutterablePerformIO $ IO $ \s ->
  case deRefWeak# w s of
    (# s1, flag, p #) -> case flag of
      0# -> (# s1, Nothing #)
      _ -> (# s1, Just (ByteArray p) #)

aliveWeaks :: NonEmpty WeakSymbol -> [ByteArray]
{-# INLINE aliveWeaks #-}
aliveWeaks = mapMaybe deRefWeakSymbol . NonEmpty.toList

-- | Get a handle to the `GlobalSymbolTable`
--
-- This can be used for pretty-printing using its `Show` instance
globalSymbolTable :: (MonadIO m) => m GlobalSymbolTable
globalSymbolTable = liftIO $ pure globalSymbolTable'

globalSymbolTable' :: GlobalSymbolTable
-- SAFETY: We need all calls to globalSymbolTable' to use the same thunk, so NOINLINE.
{-# NOINLINE globalSymbolTable' #-}
globalSymbolTable' = unsafePerformIO $ do
  !ref <- IORef.newIORef (SymbolTable mempty)
  !sipkey <- Random.uniformM Random.globalStdGen
  pure (GlobalSymbolTable ref sipkey)

-- | Returns the current size of the global symbol table. Useful for introspection or metrics.
globalSymbolTableSize :: IO Word
globalSymbolTableSize = do
  GlobalSymbolTable gsymtab _ <- globalSymbolTable
  SymbolTable table <- IORef.readIORef gsymtab
  let size = fromIntegral (IntMap.size table)
  pure size
