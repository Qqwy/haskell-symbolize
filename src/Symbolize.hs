{-| Implementation of a global Symbol Table, with garbage collection.

Symbols, also known as Atoms or Interned Strings, are a common technique
to reduce memory usage and improve performance when using many small strings.

By storing a single copy of each encountered string in a global table and giving out indexes to that table,
it is possible to compare strings for equality in constant time, instead of linear (in string size) time.

The main advantages of Symbolize over existing symbol table implementations are:

- Garbage collection: Symbols which are no longer used are automatically cleaned up.
- `Symbol`s have a memory footprint of exactly 1 `Word` and are nicely unpacked by GHC.
- Support for any `Textual` type, including `String`, (strict and lazy) `Data.Text`, (strict and lazy) `Data.ByteString` etc.
- Thread-safe.
- Calls to `lookup` and `unintern` are free of atomic memory barriers (and never have to wait on a concurrent thread running `intern`)
- Support for a maximum of 2^64 symbols at the same time (you'll probably run out of memory before that point).

== Basic usage

This module is intended to be imported qualified, e.g.



> import Symbolize (Symbol)
> import qualified Symbolize

To intern a string, use `intern`:

>>> hello = Symbolize.intern "hello"
>>> world = Symbolize.intern "world"
>>> (hello, world)
(Symbolize.intern "hello",Symbolize.intern "world")

Interning supports any `Textual` type, so you can also use `Data.Text` or `Data.ByteString` etc.:

>>> import Data.Text (Text)
>>> niceCheeses = fmap Symbolize.intern (["Roquefort", "Camembert", "Brie"] :: [Text])
>>> niceCheeses
[Symbolize.intern "Roquefort",Symbolize.intern "Camembert",Symbolize.intern "Brie"]

And if you are using OverloadedStrings, you can use the `IsString` instance to intern constants:

>>> hello2 = ("hello" :: Symbol)
>>> hello2
Symbolize.intern "hello"

Comparisons between symbols run in O(1) time:

>>> hello == hello2
True
>>> hello == world
False

To get back the textual value of a symbol, use `unintern`:

>>> Symbolize.unintern hello
"hello"

If you only want to check whether a string is already interned, use `lookup`:

>>> Symbolize.lookup "hello"
Just (Symbolize.intern "hello")

Symbols make great keys for `Data.HashMap` and `Data.HashSet`.
Hashing them is a no-op and they are guaranteed to be unique:

>>> import qualified Data.Hashable as Hashable
>>> Hashable.hash hello
0
>>> fmap Hashable.hash niceCheeses
[2,3,4]

For introspection, you can look at how many symbols currently exist:

>>> Symbolize.globalSymbolTableSize
5
>>> [unintern (intern (show x)) | x <- [1..5]]
["1","2","3","4","5"]
>>> Symbolize.globalSymbolTableSize
10

Unused symbols will be garbage-collected, so you don't have to worry about memory leaks:

>>> System.Mem.performGC
>>> Symbolize.globalSymbolTableSize
5

For deeper introspection, you can look at the Show instance of the global symbol table:
/(Note that the exact format is subject to change.)/

>>> Symbolize.globalSymbolTable
GlobalSymbolTable { count = 5, next = 10, contents = [(0,"hello"),(1,"world"),(2,"Roquefort"),(3,"Camembert"),(4,"Brie")] }
-}
module Symbolize
  ( -- * Symbol
    Symbol,
    intern,
    unintern,
    lookup,
    Textual(..),
    -- * Introspection & Metrics
    GlobalSymbolTable,
    globalSymbolTable,
    globalSymbolTableSize,
  )
where

import Control.DeepSeq (NFData(..))
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable (..))
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.String (IsString (..))
import Data.Text.Short (ShortText)
import GHC.Generics (Generic)
import GHC.Read (Read (..))
import Symbolize.Textual (Textual (..))
import qualified System.IO.Unsafe
import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak
import Text.Read (Lexeme (Ident), lexP, parens, prec, readListPrecDefault)
import qualified Text.Read
import Prelude hiding (lookup)

-- | A string-like type with O(1) equality and comparison.
--
-- A Symbol represents a string (any `Textual`, so String, Text, ByteString etc.)
-- However, it only stores an (unpacked) `Word`, used as index into a global table in which the actual string value is stored.
-- Thus equality checks are constant-time, and its memory footprint is very low.
--
-- This is very useful if you're frequently comparing strings
-- and the same strings might come up many times.
-- It also makes Symbol a great candidate for a key in a `HashMap` or `Data.HashSet`. (Hashing them is a no-op!)
--
-- The symbol table is implemented using weak pointers,
-- which means that unused symbols will be garbage collected.
-- As such, you do not need to be concerned about memory leaks.
--
-- Symbols are considered 'the same' regardless of whether they originate
-- from a `String`, (lazy or strict, normal or short) `Data.Text`, (lazy or strict, normal or short) `Data.ByteString` etc.
--
-- Symbolize supports up to 2^64 symbols existing at the same type.
-- Your system will probably run out of memory before you reach that point.
data Symbol = Symbol {-# UNPACK #-} !Word
  deriving (Generic)

instance Show Symbol where
  showsPrec p symbol =
    let !str = unintern @String symbol
     in
    showParen (p > 10) $
      showString "Symbolize.intern " . shows str

instance Read Symbol where
  readListPrec = readListPrecDefault
  readPrec = parens $ prec 10 $ do
    Ident "Symbolize" <- lexP
    Text.Read.Symbol "." <- lexP
    Ident "intern" <- lexP
    str <- readPrec @String
    return $ Symbolize.intern str

instance IsString Symbol where
  fromString = intern
  {-# INLINE fromString #-}

-- | Takes only O(1) time.
instance Eq Symbol where
  (Symbol a) == (Symbol b) = a == b
  {-# INLINE (==) #-}

-- | Symbol contains only a strict `Word`, so it is already fully evaluated.
instance NFData Symbol where
  rnf sym = seq sym ()

-- | Symbols are ordered by their `ShortText` representation.
--
-- Comparison takes O(n) time, as they are compared byte-by-byte.
instance Ord Symbol where
  compare a b = compare (unintern @ShortText a) (unintern @ShortText b)
  {-# INLINE compare #-}

-- |
-- Hashing a `Symbol` is very fast:
--
-- `hash` is a no-op and results in zero collissions, as `Symbol`'s index is unique and can be interpreted as a hash as-is.
--
-- `hashWithSalt` takes O(1) time; just as long as hashWithSalt-ing any other `Word`.
instance Hashable Symbol where
  hash (Symbol idx) = hash idx
  hashWithSalt salt (Symbol idx) = hashWithSalt salt idx
  {-# INLINE hash #-}
  {-# INLINE hashWithSalt #-}

-- | The global Symbol Table, containing a bidirectional mapping between each symbol's textual representation and its Word index.
--
-- You cannot manipulate the table itself directly,
-- but you can use `globalSymbolTable` to get a handle to it and use its `Show` instance for introspection.
--
-- `globalSymbolTableSize` can similarly be used to get the current size of the table.
data GlobalSymbolTable = GlobalSymbolTable
  { next :: !(IORef Word),
    mappings :: !(IORef SymbolTableMappings)
  }

instance Show GlobalSymbolTable where
  show table =
    -- NOTE: We want to make sure that (roughly) the same table state is used for each of the components
    -- which is why we use BangPatterns such that a partially-read show string will end up printing a (roughly) consistent state.
    let !next' = System.IO.Unsafe.unsafePerformIO $ IORef.readIORef (next table)
        !mappings' = System.IO.Unsafe.unsafePerformIO $ IORef.readIORef (mappings table)
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
  { textToSymbols :: !(HashMap ShortText (Weak Symbol)),
    symbolsToText :: !(HashMap Word ShortText)
  }

-- | Unintern a symbol, returning its textual value.
-- Takes O(log16 n) time to look up the matching textual value, where n is the number of symbols currently in the table.
--
-- Afterwards, the textual value is converted to the desired type s. See `Textual` for the type-specific time complexity.
--
-- Runs concurrently with any other operation on the symbol table, without any atomic memory barriers.
unintern :: (Textual s) => Symbol -> s
unintern (Symbol idx) =
  let !mappingsRef = mappings globalSymbolTable'
      -- SAFETY: As we only read, duplicating the IO action is benign and thus we can use unsafeDupablePerformIO here.
      !mappings' = System.IO.Unsafe.unsafeDupablePerformIO $ IORef.readIORef mappingsRef
      {-# NOINLINE mappings' #-}
   in mappings'
        & symbolsToText
        & HashMap.lookup idx
        & maybe (error "Symbol not found. This should never happen") fromShortText
{-# INLINE unintern #-}

-- | Looks up a symbol in the global symbol table.
--
-- Returns `Nothing` if no such symbol currently exists.
--
-- Takes O(log16 n) time, where n is the number of symbols currently in the table.
--
-- Runs concurrently with any other operation on the symbol table, without any atomic memory barriers.
lookup :: (Textual s) => s -> Maybe Symbol
lookup text =
  let !text' = toShortText text
      !mappingsRef = mappings globalSymbolTable'
      -- SAFETY: As we only read, duplicating the IO action is benign and thus we can use unsafeDupablePerformIO here.
      !mappings' = System.IO.Unsafe.unsafeDupablePerformIO $ IORef.readIORef mappingsRef
      {-# NOINLINE mappings' #-}
   in mappings'
        & textToSymbols
        & HashMap.lookup text'
        & lookupWeak
  where
    lookupWeak Nothing = Nothing
    lookupWeak (Just weak) = System.IO.Unsafe.unsafePerformIO $ Weak.deRefWeak weak
    {-# NOINLINE lookupWeak #-}
{-# INLINE lookup #-}

-- | Intern a string-like value.
--
-- First converts s to a `ShortText` (if it isn't already one). See `Textual` for the type-specific time complexity of this.
-- Then, takes O(log16 n) time to look up the matching symbol and insert it if it did not exist yet (where n is the number of symbols currently in the table).
--
-- Any concurrent calls to (the critical section in) `intern` are synchronized.
intern :: (Textual s) => s -> Symbol
intern text =
  let !text' = toShortText text
   in lookupOrInsert text'
  where
    lookupOrInsert text' =
      -- SAFETY: `intern` is idempotent, so inlining and CSE is benign (and might indeed improve performance).
      System.IO.Unsafe.unsafeDupablePerformIO $ IORef.atomicModifyIORef' (next globalSymbolTable') $ \next ->
        case lookup text of
          Just symbol -> (next, symbol)
          Nothing -> insert text' next
    insert text' next =
      -- SAFETY: Courtesy of atomicModifyIORef', the blackhole check is not needed as we are guaranteed to be the only thread running this code at one time.
      -- Also, since we depend on `next` we cannot flout out of the `atomicModifyIORef` lambda.
      System.IO.Unsafe.unsafeDupablePerformIO $ do
        SymbolTableMappings {symbolsToText, textToSymbols} <- IORef.readIORef (mappings globalSymbolTable')
        let !idx = nextEmptyIndex next symbolsToText
        let !symbol = Symbol idx
        weakSymbol <- Weak.mkWeakPtr symbol (Just (finalizer idx))
        let !mappings2 =
              SymbolTableMappings
                { symbolsToText = HashMap.insert idx text' symbolsToText,
                  textToSymbols = HashMap.insert text' weakSymbol textToSymbols
                }
        IORef.writeIORef (mappings globalSymbolTable') mappings2

        let !nextFree = idx + 1
        pure (nextFree, symbol)

nextEmptyIndex :: Word -> HashMap Word ShortText -> Word
nextEmptyIndex starting symbolsToText = go starting
  where
    go idx = case HashMap.lookup idx symbolsToText of
      Nothing -> idx
      _ -> go (idx + 1) -- <- Wrapping on overflow is intentional

-- | Returns a handle to the global symbol table. (Only) useful for introspection or debugging.
globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable =
  globalSymbolTable'
    -- re-introduce the IO bound which we unsafePerformIO'ed:
    & pure

globalSymbolTable' :: GlobalSymbolTable
globalSymbolTable' =
  -- SAFETY: We want all calls to globalSymbolTable' to use the same thunk, so NOINLINE.
  System.IO.Unsafe.unsafePerformIO $ do
    nextRef <- IORef.newIORef 0
    mappingsRef <- IORef.newIORef (SymbolTableMappings HashMap.empty HashMap.empty)
    return (GlobalSymbolTable nextRef mappingsRef)
{-# NOINLINE globalSymbolTable' #-}

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

finalizer :: Word -> IO ()
finalizer idx =
  IORef.atomicModifyIORef' (next globalSymbolTable') $ \next ->
    -- SAFETY: Must not be dupable
    System.IO.Unsafe.unsafePerformIO $ do
      IORef.modifyIORef' (mappings globalSymbolTable') $ \SymbolTableMappings {symbolsToText, textToSymbols} ->
        case HashMap.lookup idx symbolsToText of
          Nothing -> SymbolTableMappings {symbolsToText, textToSymbols}
          Just text ->
            SymbolTableMappings
              { symbolsToText = HashMap.delete idx symbolsToText,
                textToSymbols = HashMap.delete text textToSymbols
              }
      pure (next, ())
{-# NOINLINE finalizer #-}
