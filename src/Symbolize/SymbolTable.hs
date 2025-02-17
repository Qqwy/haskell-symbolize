{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Symbolize.SymbolTable 
( insertGlobal
, lookupGlobal
, removeGlobal
, GlobalSymbolTable
, globalSymbolTable
, globalSymbolTableSize
) where
import Prelude hiding (lookup)
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import GHC.Exts (ByteArray#, Weak#, mkWeak#, deRefWeak#)
import GHC.IO (IO(IO), unsafePerformIO)
import Data.Array.Byte (ByteArray(ByteArray))
import Data.Hashable (Hashable(hash))
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import qualified Symbolize.Accursed
import Data.Maybe (mapMaybe)
import qualified System.IO.Unsafe

data WeakSymbol where
    WeakSymbol# :: Weak# ByteArray# -> WeakSymbol

newtype SymbolTable = SymbolTable (IntMap [WeakSymbol])
newtype GlobalSymbolTable = GlobalSymbolTable (IORef SymbolTable)

newtype Hash = Hash {hashToInt :: Int}


instance Show GlobalSymbolTable where
  -- SAFETY: We're only reading, and do not care about performance here.
  show (GlobalSymbolTable table) = System.IO.Unsafe.unsafePerformIO $ do
    SymbolTable symtab <- IORef.readIORef table
    let contents = foldMap aliveWeaks $ IntMap.elems symtab
    pure $ "GlobalSymbolTable { contents = " <> show contents <> " }"

insertGlobal :: ByteArray# -> IO ByteArray
{-# INLINE insertGlobal #-}
insertGlobal ba# = do
    let !key = calculateHash ba#
    let !weak =  mkWeakSymbol ba# (removeGlobal key)
    GlobalSymbolTable gsymtab <- globalSymbolTable
    IORef.atomicModifyIORef' gsymtab $ \table ->
        case lookup ba# table of
            Just ba -> (table, ba)
            Nothing ->
                (insert key weak table, ByteArray ba#)

lookupGlobal :: ByteArray# -> IO (Maybe ByteArray)
{-# INLINE lookupGlobal #-}
lookupGlobal ba# = do
    GlobalSymbolTable gsymtab <- globalSymbolTable
    table <- IORef.readIORef gsymtab
    pure (lookup ba# table)

removeGlobal :: Hash -> IO ()
{-# INLINE removeGlobal #-}
removeGlobal !key = do
    GlobalSymbolTable gsymtab <- globalSymbolTable
    IORef.atomicModifyIORef' gsymtab $ \table ->
        (remove key table, ())

insert :: Hash -> WeakSymbol -> SymbolTable -> SymbolTable
{-# INLINE insert #-}
insert key weak (SymbolTable table) =
    let
        table' = IntMap.insertWith (++) (hashToInt key) (pure weak) table
    in SymbolTable table'

lookup :: ByteArray# -> SymbolTable -> Maybe ByteArray
{-# INLINE lookup #-}
lookup ba# (SymbolTable table) = do
    let !key = calculateHash ba#
    weaks <- IntMap.lookup (hashToInt key) table
    Foldable.find (\other -> other == ByteArray ba#) (aliveWeaks weaks)

remove :: Hash -> SymbolTable -> SymbolTable
{-# INLINE remove #-}
remove (Hash key) (SymbolTable table) = 
    let table' = IntMap.update removeTombstones key table
    in SymbolTable table'
    where
        removeTombstones weaks = 
            case filter isNoTombstone weaks of
                [] -> Nothing
                leftover -> Just leftover
        isNoTombstone weak = 
            case deRefWeakSymbol weak of
                Nothing -> False
                Just _ -> True

-- TODO: Replace with SipHash
calculateHash :: ByteArray# -> Hash
{-# INLINE calculateHash #-}
calculateHash ba# = Hash $ hash (ByteArray ba#)

mkWeakSymbol :: ByteArray# -> IO () -> WeakSymbol
{-# INLINE mkWeakSymbol #-}
mkWeakSymbol ba# (IO finalizer#) = unsafePerformIO $
    IO $ \s1 -> case mkWeak# ba# ba# finalizer# s1 of
        (# s2, weak #) -> (# s2, WeakSymbol# weak #)

deRefWeakSymbol :: WeakSymbol -> Maybe ByteArray
{-# INLINE deRefWeakSymbol #-}
deRefWeakSymbol (WeakSymbol# w) = Symbolize.Accursed.accursedUnutterablePerformIO $ IO $ \s ->
  case deRefWeak# w s of
    (# s1, flag, p #) -> case flag of
      0# -> (# s1, Nothing #)
      _ -> (# s1, Just (ByteArray p) #)

aliveWeaks :: [WeakSymbol] -> [ByteArray]
{-# INLINE aliveWeaks #-}
aliveWeaks = mapMaybe deRefWeakSymbol

globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable = pure globalSymbolTable'

globalSymbolTable' :: GlobalSymbolTable
-- SAFETY: We need all calls to globalSymbolTable' to use the same thunk, so NOINLINE.
{-# NOINLINE globalSymbolTable' #-}
globalSymbolTable' = unsafePerformIO $ do
  !ref <- IORef.newIORef (SymbolTable mempty)
  pure (GlobalSymbolTable ref)

globalSymbolTableSize :: IO Word
globalSymbolTableSize = do
    GlobalSymbolTable gsymtab <- globalSymbolTable
    SymbolTable table <- IORef.readIORef gsymtab
    let size = fromIntegral (IntMap.size table)
    pure size
