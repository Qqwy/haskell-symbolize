-- | Implementation of a global Symbol Table, with garbage collection.
-- 
-- Symbols, also known as Atoms or Interned Strings, are a common technique 
-- to reduce memory usage and improve performance when using many small strings.
--
-- By storing a single copy of each encountered string in a global table and giving out indexes to that table,
-- it is possible to compare strings for equality in constant time, instead of linear (in string size) time.
--
-- The main advantages of Symbolize over existing symbol table implementations are:
--
-- - Garbage collection: Symbols which are no longer used are automatically cleaned up.
-- - `Symbol`s have a memory footprint of exactly 1 `Word` and are nicely unpacked by GHC.
-- - Support for any `Textual` type, including `String`, (strict and lazy) `Text`, (strict and lazy) `ByteString` etc.
-- - Thread-safe.
-- - Calls to `lookup` and `unintern` are free of atomic memory barriers (and never have to wait on a concurrent thread running `intern`)
module Symbolize
  ( someFunc,
    Symbol,
    intern,
    unintern,
    lookup,
    globalSymbolTable,
    globalSymbolTableSize,
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString.Short (ShortByteString)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.String (IsString (..))
import Data.TTC (Textual)
import qualified Data.TTC as TTC
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.Read (Read (..))
import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak
import Text.Read (Lexeme (Ident), lexP, parens, prec, readListPrecDefault)
import Prelude hiding (lookup)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | A string-like type with O(1) equality and comparison.
--
-- A Symbol represents a string (any `Textual`, so String, Text, ByteString etc.)
-- However, it only stores an (unpacked) `Word`, an index into a global table in which the actual string value is stored.
-- This makes equality checks very fast.
--
-- This is very useful if you're frequently comparing strings
-- and the same strings might come up many times.
-- It also makes Symbol a great candidate for a key in a `Map` or `HashMap`.
--
-- The symbol table is implemented using weak pointers,
-- which means that unused symbols will be garbage collected.
-- As such, you do not need to be concerned about memory leaks.
--
-- Symbols are considered 'the same' regardless of whether they originate
-- from a `String`, (lazy or strict, normal or short) `Text`, (lazy or strict, normal or short) `ByteString` etc.
-- Note that if you're using `ByteString`, it is expected that it contains a valid UTF-8 encoded value beforehand.
-- This is not checked by this library. See `Textual` for more information.
--
-- Symbolize supports up to 2^64 symbols existing at the same type.
-- Your system will probably run out of memory before you reach that point.
data Symbol = Symbol {-# UNPACK #-} !Word
  deriving (Eq, Hashable, Generic, NFData)

instance Show Symbol where
  showsPrec p symbol =
    showParen (p > 10) $
      showString "Lib.intern " . shows (unintern @String symbol)

instance Read Symbol where
  -- readsPrec _ str =
  --   let sym = str & intern
  --    in [(sym, "")]
  readPrec = parens $ prec 10 $ do
    Ident "Lib.intern" <- lexP
    symbolString <- readPrec
    return (intern @ShortByteString symbolString)

  readListPrec = readListPrecDefault

instance IsString Symbol where
  fromString = intern

-- | Symbols are ordered by their `ShortByteString` representation.
-- This takes O(n) time, as they are compared byte-by-byte.
instance Ord Symbol where
  compare a b = compare (unintern @ShortByteString a) (unintern @ShortByteString b)

-- | The global Symbol Table, containing a bidirectional mapping between each symbol's textual representation and its Word index.
--
-- You cannot manipulate the table itself directly,
-- but you can use `globalSymbolTable'`, `globalSymbolTable'Size` and its `Show` instance for introspection.
--
-- `globalSymbolTable'Size` can similarly be used to get the current size of the table, also for introspective purposes.
data GlobalSymbolTable = GlobalSymbolTable
  { next :: !(IORef Word),
    mappings :: !(IORef SymbolTableMappings)
  }

instance Show GlobalSymbolTable where
  show table =
    let !next' = unsafePerformIO $ IORef.readIORef (next table)
        {-# NOINLINE next' #-}
        !mappings' = unsafePerformIO $ IORef.readIORef (mappings table)
        {-# NOINLINE mappings' #-}
        !contents = mappings' & symbolsToText
        !count = HashMap.size contents
     in "GlobalSymbolTable { count = "
          <> show count
          <> ", next = "
          <> show next'
          <> ", contents = "
          <> show (HashMap.toList contents)
          <> " }"

data SymbolTableMappings = SymbolTableMappings
  { textToSymbols :: !(HashMap ShortByteString (Weak Symbol)),
    symbolsToText :: !(HashMap Word ShortByteString)
  }

-- | Unintern a symbol, returning its textual value.
-- Takes O(log16 n) time to look up the matching textual value, where n is the number of symbols currently in the table.
--
-- Afterwards, the textual value is converted to the desired type `s`. This is a no-op if `s` is `ShortByteString`.
--
-- Runs concurrently with any other operation on the symbol table, without any atomic memory barriers.
unintern :: (Textual s) => Symbol -> s
unintern (Symbol idx) =
  let !mappingsRef = mappings globalSymbolTable'
      !mappings' = unsafePerformIO $ IORef.readIORef mappingsRef
      {-# NOINLINE mappings' #-}
   in mappings'
        & symbolsToText
        & HashMap.lookup idx
        & maybe (error "Symbol not found. This should never happen") TTC.convert

-- | Looks up a symbol in the global symbol table.
--
-- Returns `Nothing` if no such symbol currently exists.
--
-- Takes O(log16 n) time, where n is the number of symbols currently in the table.
--
-- Runs concurrently with any other operation on the symbol table, without any atomic memory barriers.
lookup :: (Textual s) => s -> Maybe Symbol
lookup text =
  let !text' = TTC.convert text
      !mappingsRef = mappings globalSymbolTable'
      !mappings' = unsafePerformIO $ IORef.readIORef mappingsRef
      {-# NOINLINE mappings' #-}
   in mappings'
        & textToSymbols
        & HashMap.lookup text'
        & lookupWeak
  where
    lookupWeak Nothing = Nothing
    lookupWeak (Just weak) = unsafePerformIO $ Weak.deRefWeak weak
    {-# NOINLINE lookupWeak #-}

-- | Intern a string-like value.
--
-- It is expected that `s` is valid UTF-8. (Which is not checked in the case of `ByteString`, c.f. `Textual`)
--
-- First converts `s` to a `ShortByteString` (if it isn't already one).
-- Then, takes O(log16 n) time to look up the matching symbol and insert it if it did not exist yet (where n is the number of symbols currently in the table).
--
-- Any concurrent calls to (the critical section in) `intern` are synchronized.
intern :: (Textual s) => s -> Symbol
intern text =
  let !text' = TTC.convert text
   in lookupOrInsert text'
  where
    lookupOrInsert text' =
      unsafePerformIO $ IORef.atomicModifyIORef' (next globalSymbolTable') $ \next ->
        case lookup text of
          Just symbol -> (next, symbol)
          Nothing -> insert text' next
    {-# NOINLINE lookupOrInsert #-}
    insert text' next =
      unsafePerformIO $ do
        let !symbol = Symbol next
        weakSymbol <- Weak.mkWeakPtr symbol Nothing
        let !next' = next + 1
        IORef.modifyIORef' (mappings globalSymbolTable') $ \SymbolTableMappings {symbolsToText, textToSymbols} ->
          SymbolTableMappings
            { symbolsToText = HashMap.insert next text' symbolsToText,
              textToSymbols = HashMap.insert text' weakSymbol textToSymbols
            }
        pure (next', symbol)


-- | Returns a handle to the global symbol table. (Only) useful for introspection or debugging.
globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable =
  globalSymbolTable'
    -- re-introduce the IO bound which we unsafePerformIO'ed:
    & pure

globalSymbolTable' :: GlobalSymbolTable
globalSymbolTable' =
  let !next = 0
      !mappings = SymbolTableMappings HashMap.empty HashMap.empty
      construct =
        unsafePerformIO $ do
          nextRef <- IORef.newIORef next
          mappingsRef <- IORef.newIORef mappings
          return (GlobalSymbolTable nextRef mappingsRef)
      {-# NOINLINE construct #-}
   in construct

-- | Returns the current size of the global symbol table. Useful for introspection or metrics.
globalSymbolTableSize :: IO Word
globalSymbolTableSize = do
  table <- globalSymbolTable
  mappings <- IORef.readIORef (mappings table)
  let size =
        mappings
          & symbolsToText
          & HashMap.size
          & fromIntegral
  pure size
