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
-- - `Symbol`s have a small memory footprint (a pinned `ByteArray#`) unpacked by GHC.
-- - Support for any `Textual` type, including `String`, (strict and lazy) `Data.Text`, (strict and lazy) `Data.ByteString` etc.
-- - Thread-safe.
-- - Efficient: 
--    - `unintern` only has to follow a pointer
--    - `lookup` reads an IOref but is free of atomic memory barries
-- - Support for as much symbols as you have memory.
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
-- Hashing them is a no-op and they are guaranteed to be unique.
-- _(Note that hash order changes between program runs, since
-- the pointer address of the pinned text bytes is internally used.)_
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
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingStrategies #-}
module Symbolize (
    -- * Symbol
    Symbol, 
    intern, 
    unintern, 
    lookup, 
    Textual (..), 

    -- * Introspection & Metrics
    GlobalSymbolTable, 
    globalSymbolTable, 
    globalSymbolTableSize
    )
    where

import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import Data.Function ((&))
import Data.String (IsString (..))
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as Text.Short
import Data.Text.Short.Unsafe qualified as Text.Short.Unsafe
import Data.Text.Display (Display (..))
import Data.ByteString.Short (ShortByteString(..))
import Data.Primitive.ByteArray (ByteArray(..), ByteArray#)
import Data.Hashable qualified as Hashable
import Control.DeepSeq (NFData (..))
import GHC.Int (Int(I#))
import GHC.Exts (addr2Int#, byteArrayContents#)
import GHC.Read (Read (..))
import Text.Read (Lexeme (Ident), lexP, parens, prec, readListPrecDefault)
import qualified Text.Read
import qualified System.IO.Unsafe
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Symbolize.Accursed
import Symbolize.Textual (Textual)
import Symbolize.Textual qualified as Textual

-- INVARIANTS:
--
-- - The ByteArray is a valid UTF-8 string and thus can be cheaply turned back into a 
--   `ShortText` simply by casting.
-- - The ByteArray is pinned, because we rely on its pointer address being stable.
newtype Symbol = Symbol ByteArray

instance Eq Symbol where
  a == b = innerByteArray# a `Symbolize.Accursed.sameByteArray` innerByteArray# b
  {-# INLINE (==) #-}

instance Ord Symbol where
    -- We do an extra ptr-equality check here,
    -- since ShortText doesn't do it
    compare a b | a == b = EQ
    compare a b = compare (symbolToShortText a) (symbolToShortText b)
    {-# INLINE compare #-}


instance Hashable.Hashable Symbol where
    hash = symbolAddrInt
    {-# INLINE hash #-}
    hashWithSalt = Hashable.defaultHashWithSalt
    {-# INLINE hashWithSalt #-}

instance Display Symbol where
    displayBuilder = unintern
    {-# INLINE displayBuilder #-}

instance Show Symbol where
    showsPrec p symbol = 
        let !str = unintern @String symbol
        in showParen (p > 10) $
            showString "Symbolize.intern " . shows str -- . showString (" " <> show (symbolAddrInt symbol))

-- | Symbol contains by definition always an already-evaluated ByteArray#
instance NFData Symbol where
  rnf sym = seq sym ()

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

instance IsString Symbol where
    fromString = intern
    {-# INLINE fromString #-}

innerByteArray# :: Symbol -> ByteArray#
innerByteArray# (Symbol (ByteArray ba#)) = ba#

symbolAddrInt :: Symbol -> Int
symbolAddrInt s = I# (addr2Int# (byteArrayContents# (innerByteArray# s)))

symbolToShortText :: Symbol -> ShortText
symbolToShortText (Symbol (ByteArray ba#)) = 
    (SBS ba#)
    & Text.Short.Unsafe.fromShortByteStringUnsafe

unintern :: (Textual str) => Symbol -> str
{-# INLINE unintern #-}
unintern s =
    s
    & symbolToShortText
    & Textual.fromShortText

intern :: (Textual str) => str -> Symbol
{-# INLINE intern #-}
intern str = 
    let !text = Textual.toShortText str in
        -- SAFETY: `intern` is idempotent, so inlining and CSE is benign (and might indeed improve performance).
        Symbolize.Accursed.accursedUnutterablePerformIO $ do
            -- Optimization: non-atomically see if the symbol already exists:
            maybeSym <- lookupInner text
            case maybeSym of
                Just sym -> pure sym
                Nothing -> 
                    -- If it doesn't, time to modify the table
                    IORef.atomicModifyIORef' (symbolTableRef globalSymbolTable') $ \symtab -> 
                        -- SAFETY: Can never float outside of the call to `atomicModifyIORef'
                        Symbolize.Accursed.accursedUnutterablePerformIO $ do
                        -- We re-check if the symbol exists, as another thread might have inserted it
                        -- in-between above check and aquiring the IORef
                        res <- lookupCritical symtab text
                        case res of
                            Just sym -> pure (symtab, sym)
                            Nothing ->
                                insert symtab text

lookup :: (Textual str) => str -> IO (Maybe Symbol)
lookup = lookupInner . Textual.toShortText

lookupInner :: ShortText -> IO (Maybe Symbol)
lookupInner text = do
    symtab <- IORef.readIORef (symbolTableRef globalSymbolTable')
    lookupCritical symtab text

lookupCritical :: SymbolTable -> ShortText -> IO (Maybe Symbol)
lookupCritical (SymbolTable table) text = 
    case Map.lookup text table of
        Nothing -> pure Nothing
        Just weak -> do
            res <- Weak.deRefWeak weak
            pure $ (fmap Symbol) res

insert :: SymbolTable -> ShortText -> IO (SymbolTable, Symbol)
insert (SymbolTable table) text = do
    let !ba = shortTextToByteArray text
    let !sym = Symbolize.Accursed.ensurePinned ba
    weak <- Symbolize.Accursed.mkWeakByteArray sym (finalizer text)
    pure (SymbolTable $ Map.insert text weak table, Symbol sym)

newtype SymbolTable = SymbolTable (Map ShortText (Weak ByteArray))
newtype GlobalSymbolTable = GlobalSymbolTable {symbolTableRef :: IORef SymbolTable}

instance Show GlobalSymbolTable where
    -- SAFETY: We're only reading, and do not care about performance here.
    show table = System.IO.Unsafe.unsafePerformIO $ do
        SymbolTable symtab <- IORef.readIORef (symbolTableRef table)
        let keys = Map.keys symtab
        pure $ "GlobalSymbolTable { contents = " <> show keys <> " }"

globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable = pure globalSymbolTable'

globalSymbolTable' :: GlobalSymbolTable
globalSymbolTable' = System.IO.Unsafe.unsafePerformIO $ do
    ref <- IORef.newIORef (SymbolTable mempty)
    pure (GlobalSymbolTable ref)

globalSymbolTableSize :: IO Word
globalSymbolTableSize = do
    table <- globalSymbolTable
    (SymbolTable symtab) <- IORef.readIORef (symbolTableRef table)
    let size = fromIntegral $ Map.size symtab
    pure size

shortTextToByteArray :: ShortText -> ByteArray
shortTextToByteArray !text = 
    let !(SBS ba#) = Text.Short.toShortByteString text
    in ByteArray ba#

finalizer :: ShortText -> IO ()
finalizer key = do
    -- putStrLn $ "Deleting key " <> (show key)
    IORef.atomicModifyIORef' (symbolTableRef globalSymbolTable') $ \(SymbolTable symtab) ->
        (SymbolTable (Map.delete key symtab), ())
{-# NOINLINE finalizer #-}
