{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable  as UM
import Data.Vector.Hashtables qualified as HashTable
import Data.List qualified
import Data.Maybe (mapMaybe)
import GHC.Exts (ByteArray#)
import GHC.IO (unsafePerformIO)
import Symbolize.Accursed qualified as Accursed
import Symbolize.SipHash qualified as SipHash
import System.IO.Unsafe qualified
import System.Random.Stateful qualified as Random (Uniform (uniformM), globalStdGen)
import Prelude hiding (lookup)
import Control.Concurrent (MVar)
import Control.Concurrent.MVar qualified as MVar
import Symbolize.WeakSymbol (WeakSymbol)
import Symbolize.WeakSymbol qualified as WeakSymbol
import Symbolize.NonEmptyWeakSymbol (NonEmptyWeakSymbol)
import Symbolize.NonEmptyWeakSymbol qualified as NonEmptyWeakSymbol

newtype SymbolTable = SymbolTable (HashTable.Dictionary (HashTable.PrimState IO) UM.MVector Int VM.MVector NonEmptyWeakSymbol)

-- | The global Symbol Table, containing a mapping between each symbol's textual representation and its deduplicated pointer.
--
-- You cannot manipulate the table itself directly,
-- but you can use `globalSymbolTable` to get a handle to it and use its `Show` instance for introspection.
--
-- `globalSymbolTableSize` can similarly be used to get the current size of the table.
--
-- Current implementation details (these might change even between PVP-compatible versions):
--
-- - A `CuckooHashTable Int` is used for mapping $(SipHash text) -> weak symbol$.
-- - Since SipHash is used as hashing algorithm and the key that is used
--   is randomized on global table initialization,
--   the table is resistent to HashDoS attacks.
data GlobalSymbolTable = GlobalSymbolTable (MVar SymbolTable) SipHash.SipKey

newtype Hash = Hash {hashToInt :: Int}

-- | What exactly this `Show` instance prints might change between PVP-compatible versions
instance Show GlobalSymbolTable where
  -- SAFETY: We're only reading, and do not care about performance here.
  show (GlobalSymbolTable table _) = System.IO.Unsafe.unsafePerformIO $ do
    MVar.withMVar table $ \(SymbolTable symtab) -> do
      elems <- fmap snd <$> HashTable.toList symtab
      let contents = Data.List.sort $ map Accursed.shortTextFromBA $ foldMap aliveWeaks elems
      pure $ "GlobalSymbolTable { size = " <> show (length contents) <> ", symbols = " <> show contents <> " }"

insertGlobal :: ByteArray# -> IO ByteArray
{-# INLINE insertGlobal #-}
insertGlobal ba# = do
  GlobalSymbolTable gsymtab sipkey <- globalSymbolTable
  let !hash = calculateHash sipkey ba#
  MVar.withMVar gsymtab $ \table -> do
    res <- lookup ba# sipkey table
    case res of
      Just ba -> pure ba
      Nothing -> do
        !weak <- WeakSymbol.new ba# (removeGlobal hash)
        insert hash weak table
        pure (ByteArray ba#)

lookupGlobal :: ByteArray# -> IO (Maybe ByteArray)
{-# INLINE lookupGlobal #-}
lookupGlobal ba# = do
  GlobalSymbolTable gsymtab sipkey <- globalSymbolTable
  MVar.withMVar gsymtab (lookup ba# sipkey)

removeGlobal :: Hash -> IO ()
{-# INLINE removeGlobal #-}
removeGlobal !key = do
  GlobalSymbolTable gsymtab _ <- globalSymbolTable
  MVar.withMVar gsymtab (remove key)

insert :: Hash -> WeakSymbol -> SymbolTable -> IO ()
{-# INLINE insert #-}
insert key weak (SymbolTable table) = HashTable.alter table insertOrConcat (hashToInt key)
  where
    insertOrConcat = \case
      Nothing -> Just (NonEmptyWeakSymbol.singleton weak)
      Just weaks -> Just (NonEmptyWeakSymbol.cons weak weaks)

lookup :: ByteArray# -> SipHash.SipKey -> SymbolTable -> IO (Maybe ByteArray)
{-# INLINE lookup #-}
lookup ba# sipkey (SymbolTable table) = do
  let !key = calculateHash sipkey ba#
  weaks <- HashTable.lookup table (hashToInt key)
  pure $ case weaks of
    Nothing -> Nothing
    Just weaks' ->
      Foldable.find (\other -> other == ByteArray ba#) (aliveWeaks weaks')

remove :: Hash -> SymbolTable -> IO ()
{-# INLINE remove #-}
remove (Hash key) (SymbolTable table) = HashTable.alter table (>>= removeTombstones) key
  where
    removeTombstones = NonEmptyWeakSymbol.nonEmpty . filter isNoTombstone . NonEmptyWeakSymbol.toList
    isNoTombstone weak =
      case WeakSymbol.deref weak of
        Nothing -> False
        Just _ -> True

calculateHash :: SipHash.SipKey -> ByteArray# -> Hash
{-# INLINE calculateHash #-}
calculateHash sipkey ba# =
  let (SipHash.SipHash word) = SipHash.hash sipkey (ByteArray ba#)
   in Hash (fromIntegral word)

aliveWeaks :: NonEmptyWeakSymbol -> [ByteArray]
{-# INLINE aliveWeaks #-}
aliveWeaks = mapMaybe WeakSymbol.deref . NonEmptyWeakSymbol.toList

-- | Get a handle to the `GlobalSymbolTable`
--
-- This can be used for pretty-printing using its `Show` instance
globalSymbolTable :: (MonadIO m) => m GlobalSymbolTable
globalSymbolTable = liftIO $ pure globalSymbolTable'

globalSymbolTable' :: GlobalSymbolTable
-- SAFETY: We need all calls to globalSymbolTable' to use the same thunk, so NOINLINE.
{-# NOINLINE globalSymbolTable' #-}
globalSymbolTable' = unsafePerformIO $ do
  !table <- HashTable.initialize 128
  !ref <- MVar.newMVar (SymbolTable table)
  !sipkey <- Random.uniformM Random.globalStdGen
  pure (GlobalSymbolTable ref sipkey)

-- | Returns the current size of the global symbol table. Useful for introspection or metrics.
--
-- Should not be used in high-performance code, as it might walk over the full table.
globalSymbolTableSize :: IO Word
globalSymbolTableSize = do
  GlobalSymbolTable gsymtab _ <- globalSymbolTable
  MVar.withMVar gsymtab $ \(SymbolTable table) -> do
    elems <- HashTable.toList table
    let size = fromIntegral (length elems)
    pure size
