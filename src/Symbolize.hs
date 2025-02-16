{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- | Implementation of a global Symbol Table, with garbage collection.
--
-- Symbols, also known as Atoms or Interned Strings, are a common technique
-- to reduce memory usage and improve performance when using many small strings.
--
-- By storing a single copy of each encountered string in a global table and giving out pointers to the stored keys,
-- it is possible to compare strings for equality in constant time, instead of linear (in string size) time.
--
-- The main advantages of Symbolize over other symbol table implementations are:
--
-- - Garbage collection: Symbols which are no longer used are automatically cleaned up.
-- - `Symbol`s have a memory footprint of exactly 1 Word and are nicely unpacked by GHC.
-- - Support for any `Textual` type, including `String`, (strict and lazy) `Data.Text`, (strict and lazy) `Data.ByteString`, `ShortText`, `ShortByteString`, etc.
-- - Thread-safe.
-- - Efficient: Calls to `lookup` and `unintern` are free of atomic memory barriers (and never have to wait on a concurrent thread running `intern`)
--
-- == Basic usage
--
-- This module is intended to be imported qualified, e.g.
--
-- > import Symbolize (Symbol)
-- > import qualified Symbolize
--
-- To intern a string, use `intern`:
--
-- >>> hello = Symbolize.intern "hello"
-- >>> world = Symbolize.intern "world"
-- >>> (hello, world)
-- (Symbolize.intern "hello",Symbolize.intern "world")
--
-- Interning supports any `Textual` type, so you can also use `Data.Text` or `Data.ByteString` etc.:
--
-- >>> import Data.Text (Text)
-- >>> niceCheeses = fmap Symbolize.intern (["Roquefort", "Camembert", "Brie"] :: [Text])
-- >>> niceCheeses
-- [Symbolize.intern "Roquefort",Symbolize.intern "Camembert",Symbolize.intern "Brie"]
--
-- And if you are using OverloadedStrings, you can use the `IsString` instance to intern constants:
--
-- >>> hello2 = ("hello" :: Symbol)
-- >>> hello2
-- Symbolize.intern "hello"
-- >>> Symbolize.intern ("world" :: Text)
-- Symbolize.intern "world"
--
-- Comparisons between symbols run in O(1) time:
--
-- >>> hello == hello2
-- True
-- >>> hello == world
-- False
--
-- To get back the textual value of a symbol, use `unintern`:
--
-- >>> Symbolize.unintern hello
-- "hello"
--
-- If you want to check whether a string is currently interned, use `lookup`:
--
-- >>> Symbolize.lookup "hello"
-- Just (Symbolize.intern "hello")
--
-- Symbols make great keys for `Data.HashMap` and `Data.HashSet`.
-- Hashing them is a no-op and they are guaranteed to be unique:
--
-- >>> Data.Hashable.hash hello
-- 1
-- >>> fmap Data.Hashable.hash niceCheeses
-- [2,3,4]
--
-- For introspection, you can look at how many symbols currently exist:
--
-- >>> Symbolize.globalSymbolTableSize
-- 5
-- >>> [unintern (intern (show x)) | x <- [1..5]]
-- ["1","2","3","4","5"]
-- >>> Symbolize.globalSymbolTableSize
-- 10
--
-- Unused symbols will be garbage-collected, so you don't have to worry about memory leaks:
--
-- >>> System.Mem.performGC
-- >>> Symbolize.globalSymbolTableSize
-- 5
--
-- For deeper introspection, you can look at the Show instance of the global symbol table:
-- /(Note that the exact format is subject to change.)/
--
-- >>> Symbolize.globalSymbolTable
-- GlobalSymbolTable { contents = ["Brie","Camembert","Roquefort","hello","world"] }
module Symbolize
  ( -- * Symbol
    Symbol,
    intern,
    unintern,
    lookup,
    Textual (..),

    -- * Introspection & Metrics
    GlobalSymbolTable,
    globalSymbolTable,
    globalSymbolTableSize,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Data.ByteString.Short (ShortByteString (SBS))
import Data.Hashable (Hashable (..))
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as Text.Short
import Data.Text.Short.Unsafe qualified as Text.Short.Unsafe
import GHC.Exts (ByteArray#, Int (I#), UnliftedType, Weak#, deRefWeak#, makeStableName#, mkWeak#, reallyUnsafePtrEquality#, stableNameToInt#)
import GHC.IO (IO (IO))
import GHC.Read (Read (..))
import Symbolize.Accursed qualified
import Symbolize.Textual (Textual)
import Symbolize.Textual qualified as Textual
import System.IO.Unsafe qualified
import Text.Read (Lexeme (Ident), lexP, parens, prec, readListPrecDefault)
import Text.Read qualified
import Prelude hiding (lookup)

-- | A string-like type with O(1) equality and comparison.
--
-- A Symbol represents a string (any `Textual`, so String, Text, ShortText, ByteString, ShortByteString, etc.)
-- However, the Symbol itself only is one word large: the pointer to an underlying `ByteArray#`.
-- A global symbol table keeps track of which values currently exist, ensuring we always deduplicate symbols.
-- This therefore allows us to:
-- - Check for equality between symbols in constant-time (using pointer equality)
-- - Calculate the hash in constant-time (using `StableName`)
-- - Keep the memory footprint of repeatedly-seen strings low.
--
-- This is very useful if you're frequently comparing strings
-- and the same strings might come up many times.
-- It also makes Symbol a great candidate for a key in a `HashMap` or `Data.HashSet`.
--
-- The global symbol table is implemented using weak pointers,
-- which means that unused symbols will be garbage collected.
-- As such, you do not need to be concerned about memory leaks (as is the case with many other symbol table implementations).
--
-- Symbols are considered 'the same' regardless of whether they originate
-- from a `String`, (lazy or strict, normal or short) `Data.Text`, (lazy or strict, normal or short) `Data.ByteString` etc.
data Symbol where
  Symbol :: {-# UNPACK #-} !Symbol# -> Symbol

-- | An unlifted version of `Symbol`;
-- this is the actual datatype we do most calculations on.
--
-- Exposed to users because it can be useful to store symbols in containers with unlifted datatypes,
-- which results in less memory usage and smaller code
-- (since the 'does this still need to be evaluated?' checks can be elided)
data Symbol# :: UnliftedType where
  Symbol# :: ByteArray# -> Symbol#

newtype WeakSymbol# = WeakSymbol# (Weak# Symbol#)

instance Show Symbol where
  showsPrec p symbol =
    let !str = unintern @ShortText symbol
     in showParen (p > 10) $
          showString "Symbolize.intern " . shows str

-- | Equality checking takes only O(1) time, and is a simple pointer-equality check.
instance Eq Symbol where
  {-# INLINE (==) #-}
  (Symbol sym1#) == (Symbol sym2#) =
    case reallyUnsafePtrEquality# sym1# sym2# of
      0# -> False
      _ -> True

-- | Symbols are ordered by their `ShortText` representation.
--
-- Comparison takes O(n) time, as they are compared byte-by-byte.
instance Ord Symbol where
  {-# INLINE compare #-}
  a `compare` b = symbolToShortText a `compare` symbolToShortText b

-- | The contents inside a `Symbol` are always guaranteed to be evaluated,
-- so we only call `seq`.
instance NFData Symbol where
  {-# INLINE rnf #-}
  rnf a = seq a ()

-- |
-- Hashing a `Symbol` is very fast:
--
-- `hash` takes O(1) and results in zero collissions, as `StableName`s are used.
--
-- `hashWithSalt` takes O(1) time; just as long as hashWithSalt-ing any other `Int`.
instance Hashable Symbol where
  {-# INLINE hash #-}
  hash = symbolHash
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt symbol = hashWithSalt salt (symbolHash symbol)

instance IsString Symbol where
  fromString = intern

-- | To be a good citizen w.r.t both `Show` and `IsString`, reading is supported two ways:
--
-- >>> read @Symbol "Symbolize.intern \"Haskell\""
-- Symbolize.intern "Haskell"
-- >>> read @Symbol "\"Curry\""
-- Symbolize.intern "Curry"
instance Read Symbol where
  readListPrec = readListPrecDefault
  readPrec = parens $ prec 10 $ full <|> onlyString
    where
      onlyString = do
        str <- readPrec @String
        return $ intern str
      full = do
        Ident "Symbolize" <- lexP
        Text.Read.Symbol "." <- lexP
        Ident "intern" <- lexP
        str <- readPrec @String
        return $ intern str

data WeakSymbol where
  WeakSymbol :: {-# UNPACK #-} !WeakSymbol# -> WeakSymbol

newtype SymbolTable = SymbolTable {unSymbolTable :: Map ShortText WeakSymbol}

newtype GlobalSymbolTable = GlobalSymbolTable {symbolTableRef :: IORef SymbolTable}

instance Show GlobalSymbolTable where
  -- SAFETY: We're only reading, and do not care about performance here.
  show table = System.IO.Unsafe.unsafePerformIO $ do
    SymbolTable symtab <- IORef.readIORef (symbolTableRef table)
    let contents = Map.keys symtab
    pure $ "GlobalSymbolTable { contents = " <> show contents <> " }"

symbolHash :: Symbol -> Int
symbolHash (Symbol sym#) =
  Symbolize.Accursed.accursedUnutterablePerformIO $ IO $ \s1 ->
    case makeStableName# sym# s1 of
      (# s2, sname# #) -> (# s2, I# (stableNameToInt# sname#) #)

-- | Looks up a symbol in the global symbol table.
--
-- Returns `Nothing` if no such symbol currently exists.
--
-- Takes O(log2(n)) time, where n is the number of symbols currently in the table.
--
-- Runs concurrently with any other operation on the symbol table, without any atomic memory barriers.
--
-- Because the result can vary depending on the current state of the symbol table, this function is not pure.
lookup :: (Textual str) => str -> IO (Maybe Symbol)
lookup str = do
  let !text = Textual.toShortText str
  GlobalSymbolTable gsymtab <- globalSymbolTable
  symtab <- IORef.readIORef gsymtab
  pure (lookupCritical text symtab)

{-# INLINE lookupCritical #-}
lookupCritical :: ShortText -> SymbolTable -> Maybe Symbol
lookupCritical !text (SymbolTable hashmap) = Map.lookup text hashmap >>= deRefWeakSymbol

-- | Intern a string-like value.
--
-- First converts s to a `ShortText` (if it isn't already one). See `Textual` for the type-specific time complexity of this.
--
-- Then, takes O(log2(n)) time to try to look up a matching symbol
-- and insert it if it did not exist yet
-- (where n is the number of symbols currently in the table).
--
-- Any concurrent calls to (the critical section in) `intern` are synchronized.
intern :: (Textual str) => str -> Symbol
{-# INLINEABLE intern #-}
intern !str =
  let !text = Textual.toShortText str
   in -- SAFETY: It is perfectly fine if floating/sharing happens
      -- because it is 'outwardly pure'
      -- furthermore, if the IO action is executed twice concurrently,
      -- synchronization will hapen at the atomicModifyIORef
      Symbolize.Accursed.accursedUnutterablePerformIO $ do
        GlobalSymbolTable gsymtab <- globalSymbolTable
        IORef.atomicModifyIORef' gsymtab $ \symtab ->
          case lookupCritical text symtab of
            Just existingSymbol -> (symtab, existingSymbol)
            Nothing -> Symbolize.Accursed.accursedUnutterablePerformIO $ do
              let !newSymbol = shortTextToNewSymbol text
              weakSymbol <- mkWeakSymbol newSymbol (finalizer newSymbol)
              let symtab' = SymbolTable $ Map.insert text weakSymbol $ unSymbolTable symtab
              pure (symtab', newSymbol)

shortTextToNewSymbol :: ShortText -> Symbol
shortTextToNewSymbol !text =
  let !(SBS ba#) = Text.Short.toShortByteString text
   in Symbol (Symbol# ba#)

mkWeakSymbol :: Symbol -> IO () -> IO WeakSymbol
mkWeakSymbol (Symbol sym#) (IO finalizer#) = do
  IO $ \s1 ->
    case mkWeak# sym# sym# finalizer# s1 of
      (# s2, weak #) -> (# s2, WeakSymbol (WeakSymbol# weak) #)

finalizer :: Symbol -> IO ()
finalizer !symbol = do
  -- putStrLn $ "Running finalizer for symbol " <> show symbol
  let !text = symbolToShortText symbol
  (GlobalSymbolTable gsymtab) <- globalSymbolTable
  IORef.atomicModifyIORef' gsymtab $ \(SymbolTable symtab) ->
    case deRefWeakSymbol <$> Map.lookup text symtab of
      Just Nothing ->
        let symtab' = Map.delete text symtab
         in (SymbolTable symtab', ())
      _ ->
        -- Nothing to do if (a) it already was removed,
        -- or (b) if it 'again exists' which means it was re-inserted
        -- in-between GC and this finalizer being triggered
        (SymbolTable symtab, ())

deRefWeakSymbol :: WeakSymbol -> Maybe Symbol
deRefWeakSymbol (WeakSymbol (WeakSymbol# w)) = Symbolize.Accursed.accursedUnutterablePerformIO $ IO $ \s ->
  case deRefWeak# w s of
    (# s1, flag, p #) -> case flag of
      0# -> (# s1, Nothing #)
      _ -> (# s1, Just (Symbol p) #)

-- | Unintern a symbol, returning its textual value.
-- Looking up the Symbol's textual value takes O(1) time, as we simply follow its internal pointer.
--
-- Afterwards, the textual value is converted to the desired type s. See `Textual` for the type-specific time complexity.
--
-- Does not use the symbol table, so runs fully concurrently with any other functions manipulating it.
unintern :: (Textual str) => Symbol -> str
{-# INLINE unintern #-}
{-# SPECIALIZE unintern :: Symbol -> ShortText #-}
unintern symbol = Textual.fromShortText (symbolToShortText symbol)

symbolToShortText :: Symbol -> ShortText
symbolToShortText (Symbol (Symbol# ba#)) =
  Text.Short.Unsafe.fromShortByteStringUnsafe (SBS ba#)

-- | The global Symbol Table, containing a mapping between each symbol's textual representation and its deduplicated pointer.
--
-- You cannot manipulate the table itself directly,
-- but you can use `globalSymbolTable` to get a handle to it and use its `Show` instance for introspection.
--
-- `globalSymbolTableSize` can similarly be used to get the current size of the table.
--
-- Current implementation details (these might change even between PVP-compatible versions):
--
-- - A (containers) `Map` is used for mapping text -> symbol. This has O(log2(n)) lookup time, but is resistent to HashDoS attacks.
globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable = pure globalSymbolTable'

globalSymbolTable' :: GlobalSymbolTable
-- SAFETY: We need all calls to globalSymbolTable' to use the same thunk, so NOINLINE.
{-# NOINLINE globalSymbolTable' #-}
globalSymbolTable' = System.IO.Unsafe.unsafePerformIO $ do
  let !set = mempty
  !ref <- IORef.newIORef (SymbolTable set)
  pure (GlobalSymbolTable ref)

-- | Returns the current size of the global symbol table. Useful for introspection or metrics.
globalSymbolTableSize :: IO Word
globalSymbolTableSize = do
  table <- globalSymbolTable
  (SymbolTable hashmap) <- IORef.readIORef (symbolTableRef table)
  pure (fromIntegral (Map.size hashmap))
