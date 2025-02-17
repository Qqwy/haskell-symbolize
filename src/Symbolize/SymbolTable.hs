{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
module Symbolize.SymbolTable where
import Prelude hiding (lookup)
import Data.Function ((&))
import Data.Foldable qualified as Foldable
import Control.Monad (filterM)
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

data WeakSymbol where
    WeakSymbol# :: Weak# ByteArray# -> WeakSymbol

newtype SymbolTable = SymbolTable (IntMap [WeakSymbol])
newtype GlobalSymbolTable = GlobalSymbolTable (IORef SymbolTable)

newtype Hash = Hash {hashToInt :: Int}

insertGlobal :: ByteArray# -> IO ByteArray
insertGlobal !ba# = do
    let !key = calculateHash ba#
    let !weak =  mkWeakSymbol ba# (removeGlobal key)
    GlobalSymbolTable gsymtab <- globalSymbolTable
    IORef.atomicModifyIORef' gsymtab $ \table ->
        case lookup ba# table of
            Just ba -> (table, ba)
            Nothing ->
                (insert key weak table, ByteArray ba#)

removeGlobal :: Hash -> IO ()
removeGlobal !key = do
    GlobalSymbolTable gsymtab <- globalSymbolTable
    IORef.atomicModifyIORef' gsymtab $ \table ->
        (remove key table, ())

lookupGlobal :: ByteArray# -> IO (Maybe ByteArray)
lookupGlobal ba# = do
    GlobalSymbolTable gsymtab <- globalSymbolTable
    table <- IORef.readIORef gsymtab
    pure (lookup ba# table)


lookup :: ByteArray# -> SymbolTable -> Maybe ByteArray
lookup ba# (SymbolTable table) = do
    let !key = calculateHash ba#
    weaks <- IntMap.lookup (hashToInt key) table
    Foldable.find (\other -> other == ByteArray ba#) (alives weaks)


-- TODO: Replace with SipHash
calculateHash :: ByteArray# -> Hash
calculateHash ba# = Hash $ hash (ByteArray ba#)

mkWeakSymbol :: ByteArray# -> IO () -> WeakSymbol
mkWeakSymbol ba# (IO finalizer#) = unsafePerformIO $
    IO $ \s1 -> case mkWeak# ba# ba# finalizer# s1 of
        (# s2, weak #) -> (# s2, WeakSymbol# weak #)


insert :: Hash -> WeakSymbol -> SymbolTable -> SymbolTable
insert key weak (SymbolTable table) =
    let
        table' = IntMap.insertWith (++) (hashToInt key) (pure weak) table
    in SymbolTable table'

remove :: Hash -> SymbolTable -> SymbolTable
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

deRefWeakSymbol :: WeakSymbol -> Maybe ByteArray
{-# INLINE deRefWeakSymbol #-}
deRefWeakSymbol (WeakSymbol# w) = Symbolize.Accursed.accursedUnutterablePerformIO $ IO $ \s ->
  case deRefWeak# w s of
    (# s1, flag, p #) -> case flag of
      0# -> (# s1, Nothing #)
      _ -> (# s1, Just (ByteArray p) #)

alives :: [WeakSymbol] -> [ByteArray]
alives = mapMaybe deRefWeakSymbol

globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable = pure globalSymbolTable'

globalSymbolTable' :: GlobalSymbolTable
-- SAFETY: We need all calls to globalSymbolTable' to use the same thunk, so NOINLINE.
{-# NOINLINE globalSymbolTable' #-}
globalSymbolTable' = unsafePerformIO $ do
  !ref <- IORef.newIORef (SymbolTable mempty)
  pure (GlobalSymbolTable ref)
