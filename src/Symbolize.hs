-- | Symbols AKA Atoms AKA Interned Strings, with garbage collection.
module Symbolize (Symbol (..), GlobalSymbolTable, intern, lookup, unintern, globalSymbolTable, globalSymbolTableSize) where

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.DeepSeq (NFData)
import Data.ByteString.Short (ShortByteString)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
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
      showString "Symbolize.intern " . shows (unintern @String symbol)

instance Read Symbol where
  -- readsPrec _ str =
  --   let sym = str & intern
  --    in [(sym, "")]
  readPrec = parens $ prec 10 $ do
    Ident "Symbolize.intern" <- lexP
    symbolString <- readPrec
    return (intern @ShortByteString symbolString)

  readListPrec = readListPrecDefault

instance IsString Symbol where
  fromString = intern

-- | Symbols are ordered by their `ShortByteString` representation.
-- This takes O(n) time, as they are compared byte-by-byte.
instance Ord Symbol where
  compare a b = compare (unintern @ShortByteString a) (unintern @ShortByteString b)

-- | Intern a string-like value.
--
-- It is expected that `s` is valid UTF-8. (Which is not checked in the case of `ByteString`, c.f. `Textual`)
intern :: (Textual s) => s -> Symbol
intern text =
  -- Force evaluation before running atomic section:
  let !key = TTC.convert text
   in unsafePerformIO $ do
        table <- MVar.takeMVar globalSymbolTable'
        maybe_symbol <- lookupWeak $ HashMap.lookup key $ textToSymbols table
        case maybe_symbol of
          Just symbol -> do
            MVar.putMVar globalSymbolTable' table
            pure symbol
          Nothing -> do
            (table', symbol) <- unsafeAddSymbol table key
            MVar.putMVar globalSymbolTable' table'
            pure symbol
  where
    lookupWeak Nothing = pure Nothing
    lookupWeak (Just val) = Weak.deRefWeak val

-- | Looks up a symbol in the global symbol table.
--
-- Returns `Nothing` if no such symbol currently exists.
lookup :: (Textual s) => s -> Maybe Symbol
lookup text =
  -- Force evaluation before running atomic section:
  let !key = TTC.convert text
   in globalSymbolTable'
        & readSymbolTable
        & textToSymbols
        & HashMap.lookup key
        & \case
          Nothing -> Nothing
          Just weak_symbol -> unsafePerformIO (Weak.deRefWeak weak_symbol)

-- | Unintern a symbol, returning its textual value.
unintern :: (Textual s) => Symbol -> s
unintern (Symbol idx) =
  globalSymbolTable'
    & readSymbolTable
    & symbolsToText
    & HashMap.lookup idx
    & \case
      Nothing -> error "Symbol not found. This should never happen."
      Just text -> TTC.convert text

data SymbolTable = SymbolTable
  { next :: {-# UNPACK #-} !Word,
    textToSymbols :: !(HashMap ShortByteString (Weak Symbol)),
    symbolsToText :: !(HashMap Word ShortByteString)
  }

instance Show SymbolTable where
  show env =
    let count = HashMap.size (textToSymbols env)
        contents = env & symbolsToText & HashMap.toList & fmap (\(idx, text) -> show idx <> " <-> " <> show text) & unlines
     in "SymbolTable { next = " <> show (next env) <> ", count = " <> show count <> ", contents = [\n" <> contents <> "] }"

-- | The global Symbol Table, containing a bidirectional mapping between each symbol's textual representation and its Word index.
--
-- You cannot manipulate the table itself directly,
-- but you can use `globalSymbolTable`, `globalSymbolTableSize` and its `Show` instance for introspection.
--
-- `globalSymbolTableSize` can similarly be used to get the current size of the table, also for introspective purposes.
newtype GlobalSymbolTable = GlobalSymbolTable (MVar SymbolTable)

-- | Prints a representation of the global symbol table,
-- which includes the full mapping of symbols currently inside.
--
-- **Only use this for introspection**! The format of the returned string is subject to change.
instance Show GlobalSymbolTable where
  show (GlobalSymbolTable mutableTable) =
    mutableTable
      & readSymbolTable
      & show

-- | Returns a handle to the global symbol table. (Only) useful for introspection or debugging.
globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable = pure $ GlobalSymbolTable globalSymbolTable'

-- | Returns the current size of the global symbol table. Useful for introspection or metrics.
globalSymbolTableSize :: IO Word
globalSymbolTableSize =
  globalSymbolTable'
    & readSymbolTable
    & symbolsToText
    & HashMap.size
    & fromIntegral
    & pure

globalSymbolTable' :: MVar SymbolTable
globalSymbolTable' = unsafePerformIO $ MVar.newMVar newSymbolTable
{-# NOINLINE globalSymbolTable' #-}

readSymbolTable :: MVar SymbolTable -> SymbolTable
readSymbolTable mutableTable =
  mutableTable
    & MVar.readMVar
    & unsafePerformIO
{-# NOINLINE readSymbolTable #-}

newSymbolTable :: SymbolTable
newSymbolTable =
  SymbolTable
    { next = 0,
      textToSymbols = HashMap.empty,
      symbolsToText = HashMap.empty
    }

unsafeAddSymbol :: SymbolTable -> ShortByteString -> IO (SymbolTable, Symbol)
unsafeAddSymbol table text = do
  let idx = nextEmptyIndex table
  let symbol = Symbol idx
  weakSymbol <- Weak.mkWeakPtr symbol (Just (finalizer globalSymbolTable' idx))
  let next' = idx + 1 -- <- Wrapping on overflow is intentional
  let textToSymbols' =
        table
          & textToSymbols
          & HashMap.insert text weakSymbol
  let symbolsToText' =
        table
          & symbolsToText
          & HashMap.insert idx text
  pure (SymbolTable {next = next', textToSymbols = textToSymbols', symbolsToText = symbolsToText'}, symbol)
{-# NOINLINE unsafeAddSymbol #-}

nextEmptyIndex :: SymbolTable -> Word
nextEmptyIndex env = go (next env)
  where
    go idx = case HashMap.lookup idx (symbolsToText env) of
      Nothing -> idx
      _ -> go (idx + 1) -- <- Wrapping on overflow is intentional

finalizer :: MVar SymbolTable -> Word -> IO ()
finalizer mvar idx =
  MVar.modifyMVar_ mvar $ \env' ->
    pure (removeSymbol idx env')
  where
    removeSymbol :: Word -> SymbolTable -> SymbolTable
    removeSymbol index env = case HashMap.lookup idx (symbolsToText env) of
      Nothing -> env
      Just text ->
        env
          { textToSymbols = HashMap.delete text (textToSymbols env),
            symbolsToText = HashMap.delete index (symbolsToText env)
          }
