{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- | Symbols AKA Atoms AKA Interned Strings, with garbage collection.
--
--
module Symbolize (Symbol(..), SymbolTable, intern, internSafe, unintern, globalSymbolTable, globalSymbolTableSize) where

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.String (IsString (..))
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak
import Data.ByteString.Short (ShortByteString)
import Data.TTC (Textual)
import qualified Data.TTC as TTC
import Control.DeepSeq (NFData)
import GHC.Read (Read(..))
import Text.Read (Lexeme(Ident), parens, prec, lexP, readListPrecDefault)

-- | A string-like type with O(1) equality and comparison. 
--
-- A Symbol represents a string ([String]/[Text]/[ByteString]).
-- However, it only stores an (unpacked) [Word], making equality checks very fast.
--
-- This is very useful if you're frequently comparing strings
-- and the same strings might come up many times.
-- It also makes Symbol a great candidate for a key in a [Map] or [HashMap].
--
-- The symbol table is implemented using weak pointers,
-- which means that unused symbols will be garbage collected.
-- As such, you do not need to be concerned about memory leaks.
--
-- Symbols are considered 'the same' regardless of whether they originate
-- from a [String], (lazy or strict, normal or short) [Text], (lazy or strict, normal or short) [ByteString] etc.
-- Note that if you're using [ByteString], it is expected that it contains a valid UTF-8 encoded value beforehand.
-- This is not checked by this library. See [Textual] for more information.
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

-- | Symbols are ordered by their [ShortByteString] representation. 
-- This takes O(n) time, as they are compared byte-by-byte.
instance Ord Symbol where
  compare a b = compare (unintern @ShortByteString a) (unintern @ShortByteString b)


intern :: Textual s => s -> Symbol
intern text = case internSafe text of
  Just symbol -> symbol
  Nothing ->
    unsafeAddSymbol globalSymbolTable (TTC.convert text)

internSafe :: Textual s => s -> Maybe Symbol
internSafe text =
  globalSymbolTable
    & readSymbolTable
    & textToSymbols
    & HashMap.lookup (TTC.convert text)
    & \case
      Nothing -> Nothing
      Just weak_symbol -> unsafePerformIO (Weak.deRefWeak weak_symbol)

unintern :: Textual s => Symbol -> s
unintern (Symbol idx) =
  globalSymbolTable
    & readSymbolTable
    & symbolsToText
    & HashMap.lookup idx
    & \case
      Nothing -> error "Symbol not found. This should never happen."
      Just text -> TTC.convert text

-- | Table containing the symbols.
--
-- There is only one, called [globalSymbolTable].
--
-- You cannot manipulate the table itself directly,
-- but it has a [Show] instance which you can use for introspection.
--
-- [globalSymbolTableSize] can similarly be used to get the current size of the table, also for introspective purposes.
data SymbolTable = SymbolTable
  { next :: Word,
    textToSymbols :: HashMap ShortByteString (Weak Symbol),
    symbolsToText :: HashMap Word ShortByteString
  }

instance Show SymbolTable where
  show env =
    let count = HashMap.size (textToSymbols env)
        contents = env & symbolsToText & HashMap.toList & fmap (\(idx, text) -> show idx <> " <-> " <> show text) & unlines
     in "SymbolTable { next = " <> show (next env) <> ", count = " <> show count <> ", contents = [\n" <> contents <> "] }"

instance Show (MVar SymbolTable) where
  show = show . readSymbolTable

globalSymbolTableSize :: IO Int
globalSymbolTableSize = pure $ globalSymbolTable & readSymbolTable & symbolsToText & HashMap.size

globalSymbolTable :: MVar SymbolTable
globalSymbolTable = unsafePerformIO $ MVar.newMVar newSymbolTable
{-# NOINLINE globalSymbolTable #-}

readSymbolTable :: MVar SymbolTable -> SymbolTable
readSymbolTable mutableTable =
  mutableTable
    & MVar.readMVar
    & unsafePerformIO

newSymbolTable :: SymbolTable
newSymbolTable =
  SymbolTable
    { next = 0,
      textToSymbols = HashMap.empty,
      symbolsToText = HashMap.empty
    }

unsafeAddSymbol :: MVar SymbolTable -> ShortByteString -> Symbol
unsafeAddSymbol mutableTable text = unsafePerformIO $ MVar.modifyMVar mutableTable $ \env -> do
  let idx = nextEmptyIndex env
  let symbol = Symbol idx
  weakSymbol <- Weak.mkWeakPtr symbol (Just (finalizer mutableTable idx))
  let next' = idx + 1 -- <- Wrapping on overflow is intentional
  let textToSymbols' =
        env
          & textToSymbols
          & HashMap.insert text weakSymbol
  let symbolsToText' =
        env
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

