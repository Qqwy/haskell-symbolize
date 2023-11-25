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
-- - Support for any `Textual` type, including `String`, (strict and lazy) `Data.Text`, (strict and lazy) `Data.ByteString` etc.
-- - Thread-safe.
-- - Efficient: Calls to `lookup` and `unintern` are free of atomic memory barriers (and never have to wait on a concurrent thread running `intern`)
-- - Support for a maximum of 2^64 symbols at the same time (you'll probably run out of memory before that point).
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
-- 0
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
-- GlobalSymbolTable { count = 5, next = 10, contents = [(0,"hello"),(1,"world"),(2,"Roquefort"),(3,"Camembert"),(4,"Brie")] }
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
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.DeepSeq (NFData (..))
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Hashable (Hashable (..))
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.String (IsString (..))
import Data.Text.Display (Display (..))
import Data.Text.Short (ShortText)
import GHC.Read (Read (..))
import qualified Symbolize.Accursed
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

instance Show Symbol where
  showsPrec p symbol =
    let !str = unintern @String symbol
     in showParen (p > 10) $
          showString "Symbolize.intern " . shows str

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
        return $ Symbolize.intern str
      full = do
        Ident "Symbolize" <- lexP
        Text.Read.Symbol "." <- lexP
        Ident "intern" <- lexP
        str <- readPrec @String
        return $ Symbolize.intern str

instance IsString Symbol where
  fromString = intern
  {-# INLINE fromString #-}

-- |
-- >>> Data.Text.Display.display (Symbolize.intern "Pizza")
-- "Pizza"
instance Display Symbol where
  displayBuilder = unintern
  {-# INLINE displayBuilder #-}

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
--
-- Current implementation details (these might change even between PVP-compatible versions):
-- 
-- - A (containers) `Map` is used for mapping text -> symbol. This has O(log2(n)) lookup time, but is resistent to HashDoS attacks.
-- - A (unordered-containers) `HashMap` is used for mapping symbol -> text. This has O(log16(n)) lookup time. 
--   Because symbols are unique and their values are not user-generated, there is no danger of HashDoS here.
data GlobalSymbolTable = GlobalSymbolTable
  { next :: !(MVar Word),
    mappings :: !(IORef SymbolTableMappings)
  }

instance Show GlobalSymbolTable where
  show table =
    -- SAFETY: We're only reading, and do not care about performance here.
    System.IO.Unsafe.unsafePerformIO $ do
      -- NOTE: We want to make sure that (roughly) the same table state is used for each of the components
      -- which is why we use BangPatterns such that a partially-read show string will end up printing a (roughly) consistent state.
      !next' <- MVar.readMVar (next table) -- IORef.readIORef (next table)
      !mappings' <- IORef.readIORef (mappings table)
      let !contents = mappings' & symbolsToText
      -- let !reverseContents = mappings' & textToSymbols & fmap  (fmap hash . System.IO.Unsafe.unsafePerformIO . Weak.deRefWeak) & HashMap.toList
      let !count = HashMap.size contents
      pure
        $ "GlobalSymbolTable { count = "
          <> show count
          <> ", next = "
          <> show next'
          -- <> ", reverseContents = "
          -- <> show reverseContents
          <> ", contents = "
          <> show (HashMap.toList contents)
          <> " }"

data SymbolTableMappings = SymbolTableMappings
  { textToSymbols :: !(Map ShortText (Weak Symbol)),
    symbolsToText :: !(HashMap Word ShortText)
  }

-- | Unintern a symbol, returning its textual value.
-- Takes O(log16(n)) time to look up the matching textual value, where n is the number of symbols currently in the table.
--
-- Afterwards, the textual value is converted to the desired type s. See `Textual` for the type-specific time complexity.
--
-- Runs concurrently with any other operation on the symbol table, without any atomic memory barriers.
unintern :: (Textual s) => Symbol -> s
unintern (Symbol idx) =
  let !mappingsRef = mappings globalSymbolTable'
      -- SAFETY:
      -- First, it's thread-safe because we only read (from a single IORef).
      -- Second, this function is idempotent and (outwardly) pure,
      -- so whether it is executed only once or many times for a particular Symbol does not matter in the slightest.
      -- Thus, we're very happy with the compiler inlining, CSE'ing or floating out this IO action.
      --
      -- I hope I'm correct and the Cosmic Horror will not eat me!
      -- signed by Marten, 2023-11-24
      !mappings' = Symbolize.Accursed.accursedUnutterablePerformIO $ IORef.readIORef mappingsRef
   in mappings'
        & symbolsToText
        & HashMap.lookup idx
        & maybe (error ("Symbol " <> show idx <> " not found. This should never happen" <> show globalSymbolTable')) fromShortText
{-# INLINE unintern #-}

-- | Looks up a symbol in the global symbol table.
--
-- Returns `Nothing` if no such symbol currently exists.
--
-- Takes O(log2(n)) time, where n is the number of symbols currently in the table.
--
-- Runs concurrently with any other operation on the symbol table, without any atomic memory barriers.
--
-- Because the result can vary depending on the current state of the symbol table, this function is not pure.
lookup :: (Textual s) => s -> IO (Maybe Symbol)
lookup text = do
  let !text' = toShortText text
  table <- globalSymbolTable
  mappings <- IORef.readIORef (mappings table)
  let maybeWeak = mappings & textToSymbols & Map.lookup text'
  case maybeWeak of
    Nothing -> pure Nothing
    Just weak -> do
      Weak.deRefWeak weak

-- | Intern a string-like value.
--
-- First converts s to a `ShortText` (if it isn't already one). See `Textual` for the type-specific time complexity of this.
-- Then, takes O(log2(n)) time to look up the matching symbol and insert it if it did not exist yet (where n is the number of symbols currently in the table).
--
-- Any concurrent calls to (the critical section in) `intern` are synchronized.
intern :: (Textual s) => s -> Symbol
intern text =
  let !text' = toShortText text
   in lookupOrInsert text'
  where
    lookupOrInsert text' =
      -- SAFETY: `intern` is idempotent, so inlining and CSE is benign (and might indeed improve performance).
      System.IO.Unsafe.unsafePerformIO $ MVar.modifyMVar (next globalSymbolTable') $ \next -> do
        maybeWeak <- lookup text
        case maybeWeak of
          Just symbol -> pure (next, symbol)
          Nothing -> insert text' next
    insert text' next = do
      SymbolTableMappings {symbolsToText, textToSymbols} <- IORef.readIORef (mappings globalSymbolTable')
      let !idx = nextEmptyIndex next symbolsToText
      let !symbol = Symbol idx
      weakSymbol <- Weak.mkWeakPtr symbol (Just (finalizer idx))
      let !mappings2 =
            SymbolTableMappings
              { symbolsToText = HashMap.insert idx text' symbolsToText,
                textToSymbols = Map.insert text' weakSymbol textToSymbols
              }
      IORef.atomicWriteIORef (mappings globalSymbolTable') mappings2

      let !nextFree = idx + 1
      pure (nextFree, symbol)
{-# INLINE intern #-}

nextEmptyIndex :: Word -> HashMap Word ShortText -> Word
nextEmptyIndex starting symbolsToText = go starting
  where
    go idx = case HashMap.lookup idx symbolsToText of
      Nothing -> idx
      _ -> go (idx + 1) -- <- Wrapping on overflow is intentional and important for correctness

-- | Returns a handle to the global symbol table. (Only) useful for introspection or debugging.
globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable =
  globalSymbolTable'
    -- re-introduce the IO bound which we unsafePerformIO'ed:
    & pure

globalSymbolTable' :: GlobalSymbolTable
globalSymbolTable' =
  -- SAFETY: We need all calls to globalSymbolTable' to use the same thunk, so NOINLINE.
  System.IO.Unsafe.unsafePerformIO $ do
    nextRef <- MVar.newMVar 0 -- IORef.newIORef 0
    mappingsRef <- IORef.newIORef (SymbolTableMappings Map.empty HashMap.empty)
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
finalizer idx = do
  MVar.withMVar (next globalSymbolTable') $ \_next -> do
    IORef.modifyIORef' (mappings globalSymbolTable') $ \SymbolTableMappings {symbolsToText, textToSymbols} ->
      case HashMap.lookup idx symbolsToText of
        Nothing -> error ("Duplicate finalizer called for " <> show idx <> "This should never happen") -- SymbolTableMappings {symbolsToText, textToSymbols}
        Just text ->
          SymbolTableMappings
            { symbolsToText = HashMap.delete idx symbolsToText,
              textToSymbols = Map.delete text textToSymbols
            }
{-# NOINLINE finalizer #-}