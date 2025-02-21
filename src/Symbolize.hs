{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- | Implementation of a global Symbol Table, with garbage collection.
--
-- Symbols, also known as Atoms or Interned Strings, are a common technique
-- to reduce memory usage and improve performance when using many small strings:
--
-- A Symbol represents a string (any `Textual`, so String, Text, ShortText, ByteString, ShortByteString, etc.)
--
-- Just like `ShortText`, `ShortByteString` and `ByteArray`, a `Symbol` has an optimized memory representation,
-- directly wrapping a primitive `ByteArray#`.
--
-- Furthermore, a global symbol table keeps track of which values currently exist, ensuring we always deduplicate symbols.
-- This therefore allows us to:
-- - Check for equality between symbols in constant-time (using pointer equality)
-- - Calculate the hash in constant-time (using `StableName`)
-- - Keep the memory footprint of repeatedly-seen strings low.
--
-- This is very useful if you're frequently comparing strings
-- and the same strings might come up many times.
-- It also makes Symbol a great candidate for a key in e.g. a `HashMap` or `HashSet`.
--
-- The global symbol table is implemented using weak pointers,
-- which means that unused symbols will be garbage collected.
-- As such, you do not need to be concerned about memory leaks
-- (as is the case with many other symbol table implementations).
--
-- Symbols are considered 'the same' regardless of whether they originate
-- from a `String`, (lazy or strict, normal or short) `Data.Text`, (lazy or strict, normal or short) `Data.ByteString` etc.
--
-- The main advantages of Symbolize over other symbol table implementations are:
--
-- - Garbage collection: Symbols which are no longer used are automatically cleaned up.
-- - Support for any `Textual` type, including `String`, (strict and lazy) `Data.Text`, (strict and lazy) `Data.ByteString`, `ShortText`, `ShortByteString`, etc.
-- - Great memory usage:
--    - `Symbol`s are simply a (lifted) wrapper around a `ByteArray#`, which is nicely unpacked by GHC.
--    - The symbol table is an `IntMap` that contains weak pointers to these same `ByteArray#`s and their associated `StableName#`s
-- - Great performance:
--   - `unintern` is a simple pointer-dereference
--   - calls to `lookup` are free of atomic memory barriers (and never have to wait on a concurrent thread running `intern`)
-- - Thread-safe
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
-- And if you are using `OverloadedStrings`, you can use the `IsString` instance to intern constants:
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
-- Hashing them takes constant-time and they are guaranteed to be unique:
--
-- >>> Data.Hashable.hash hello
-- 1
-- >>> Data.Hashable.hash world
-- 2
-- >>> fmap Data.Hashable.hash niceCheeses
-- [3,4,5]
--
-- For introspection, you can look at how many symbols currently exist:
--
-- >>> System.Mem.performGC
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
-- GlobalSymbolTable { size = 5, symbols = ["Brie","Camembert","Roquefort","hello","world"] }
module Symbolize
  ( -- * Symbol
    Symbol (..),
    intern,
    internUnsafe,
    unintern,
    lookup,

    -- * Introspection & Metrics
    SymbolTable.GlobalSymbolTable,
    SymbolTable.globalSymbolTable,
    SymbolTable.globalSymbolTableSize,

    -- * manipulate unlifted Symbols directly
    Symbol#,
    intern#,
    internUnsafe#,
    intern##,
    unintern#,
    unintern##,
    sameSymbol#,
    sameSymbol##,
    hashSymbol#,
    hashSymbol##,
    compareSymbol#,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (rnf))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Array.Byte (ByteArray (ByteArray))
import Data.ByteString.Short (ShortByteString (SBS))
import Data.Function ((&))
import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.String (IsString (fromString))
import Data.Text.Short qualified as Text.Short
import Data.Text.Short.Unsafe qualified as Text.Short.Unsafe
import GHC.Exts (ByteArray#, Int#, sameByteArray#)
import GHC.Int (Int (I#))
import Symbolize.Accursed qualified as Accursed
import Symbolize.SymbolTable qualified as SymbolTable
import Symbolize.Textual (Textual)
import Symbolize.Textual qualified as Textual
import Text.Read (Lexeme (Ident), Read (..), lexP, parens, prec, readListPrecDefault)
import Text.Read qualified
import Prelude hiding (lookup)
import Data.Data qualified
import Data.Binary qualified

-- | A string-like type with O(1) equality and comparison.
--
-- A Symbol represents a string (any `Textual`, so String, Text, ShortText, ByteString, ShortByteString, etc.)
--
-- Just like `ShortText`, `ShortByteString` and `ByteArray`, a `Symbol` has an optimized memory representation,
-- directly wrapping a primitive `ByteArray#`.
--
-- Furthermore, a global symbol table keeps track of which values currently exist, ensuring we always deduplicate symbols.
-- This therefore allows us to:
-- - Check for equality between symbols in constant-time (using pointer equality)
-- - Calculate the hash in constant-time (using `StableName`)
-- - Keep the memory footprint of repeatedly-seen strings low.
--
-- This is very useful if you're frequently comparing strings
-- and the same strings might come up many times.
-- It also makes Symbol a great candidate for a key in e.g. a `HashMap` or `HashSet`.
--
-- The global symbol table is implemented using weak pointers,
-- which means that unused symbols will be garbage collected.
-- As such, you do not need to be concerned about memory leaks
-- (as is the case with many other symbol table implementations).
--
-- Symbols are considered 'the same' regardless of whether they originate
-- from a `String`, (lazy or strict, normal or short) `Data.Text`, (lazy or strict, normal or short) `Data.ByteString` etc.
data Symbol where
  Symbol :: Symbol# -> Symbol

-- | Unlifted version of `Symbol`
--
-- This is of kind `UnliftedType` (AKA `TYPE (BoxedRep Unlifted)`)
-- which means GHC knows it is already fully-evaluated
-- and can never contain bottoms.
--
-- In many cases, GHC is able to figure out
-- that the symbols you're using are already-evaluated
-- and will unbox them into `Symbol#`s automatically
-- behind the scenes.
--
-- However, in some cases, directly manipulating a `Symbol#`
-- can be beneficial, such as when storing them as keys or values
-- inside a collection that supports `UnliftedType`s.
--
-- A `Symbol#` has exactly the same in-memory representation
-- as a `ByteArray#` (It is an unlifted newtype around `ByteArray#`).
newtype Symbol# = Symbol# ByteArray#

-- | Equality checking takes only O(1) time, and is a simple pointer-equality check.
instance Eq Symbol where
  {-# INLINE (==) #-}
  (Symbol a) == (Symbol b) = a `sameSymbol#` b

-- | Symbols are ordered by their lexicographical UTF-8 representation.
--
-- Therefore, comparison takes O(n) time.
instance Ord Symbol where
  {-# INLINE compare #-}
  (Symbol a) `compare` (Symbol b) = a `compareSymbol#` b

instance Show Symbol where
  showsPrec p symbol =
    let !str = unintern @String symbol
     in showParen (p > 10) $
          showString "Symbolize.intern " . shows str

instance IsString Symbol where
  {-# INLINE fromString #-}
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

-- | The contents inside a `Symbol` are always guaranteed to be evaluated,
-- so this only forces the outernmost constructor using `seq`.
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
  hash (Symbol s) = hashSymbol# s
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (Symbol s) = hashWithSalt salt (hashSymbol# s)

-- | Intern a string-like value.
--
-- First converts the string to a `ShortText` (if it isn't already one).
-- See `Textual` for the type-specific time complexity of this.
--
-- Finally, it takes O(min(n, 64)) time to try to look up a matching symbol
-- and insert it if it did not exist yet
-- (where n is the number of symbols currently in the table).
--
-- Any concurrent calls to (the critical section in) `intern` are synchronized.
intern :: (Textual str) => str -> Symbol
{-# INLINE intern #-}
intern !str = Symbol (intern# str)

-- | Version of `intern` that returns an unlifted `Symbol#`
intern# :: (Textual str) => str -> Symbol#
{-# INLINE intern# #-}
intern# !str = intern## (textualToBA# str)


-- | Like `intern`, but skips any potential UTF-8 validation
internUnsafe :: (Textual str) => str -> Symbol
{-# INLINE internUnsafe #-}
internUnsafe !str = Symbol (internUnsafe# str)

-- | Like `intern#`, but skips any potential UTF-8 validation
internUnsafe# :: (Textual str) => str -> Symbol#
{-# INLINE internUnsafe# #-}
internUnsafe# !str = intern## (textualToBAUnsafe# str)

{-# DEPRECATED intern## "Renamed to `internUnsafe##`" #-}
intern## :: ByteArray# -> Symbol#
{-# INLINE intern## #-}
intern## = internUnsafe##

-- | Version of `intern`/`internUnsafe` that directly works on an unlifted `ByteArray#` and returns an unlifted `Symbol#`
--
-- Does not do any checking for UTF-8 validity on the `ByteArray#` beforehand.
internUnsafe## :: ByteArray# -> Symbol#
{-# INLINE internUnsafe## #-}
internUnsafe## ba# =
  -- SAFETY: We're actually happy with let-floating/combining
  -- since the result is 'outwardly pure' and doing less work is better!
  let !(ByteArray ba2#) = Accursed.accursedUnutterablePerformIO (SymbolTable.insertGlobal ba#)
   in Symbol# ba2#

-- | Looks up a symbol in the global symbol table.
--
-- Returns `Nothing` if no such symbol currently exists.
--
-- First converts the string to a `ShortText` (if it isn't already one).
-- See `Textual` for the type-specific time complexity of this.
--
-- Then, takes O(min(n, 64)) time, where n is the number of symbols currently in the table.
--
-- Runs concurrently with any other operation on the symbol table, without any atomic memory barriers.
--
-- Because the result can vary depending on the current state of the symbol table, this function is not pure.
lookup :: (Textual str, MonadIO m) => str -> m (Maybe Symbol)
{-# INLINE lookup #-}
lookup !str = lookup# (textualToBA# str)

-- | Version of `lookup` that directly works on an unlifted `ByteArray#`
lookup# :: (MonadIO m) => ByteArray# -> m (Maybe Symbol)
{-# INLINE lookup# #-}
lookup# ba# =
  liftIO $
    SymbolTable.lookupGlobal ba#
      & fmap (fmap (\(ByteArray ba2#) -> Symbol (Symbol# ba2#)))

-- | Unintern a symbol, returning its textual value.
--
-- Looking up the Symbol's textual value takes O(1) time, as we simply follow its internal pointer.
--
-- Afterwards, the textual value is converted to the desired string type.
-- See `Textual` for the type-specific time complexity of this.
--
-- Does not use the symbol table, so runs fully concurrently with any other functions manipulating it.
unintern :: (Textual str) => Symbol -> str
{-# INLINE unintern #-}
unintern (Symbol sym#) = unintern# sym#

-- | Version of `unintern` that works directly on an unlifted `Symbol#`
unintern# :: (Textual str) => Symbol# -> str
{-# INLINE unintern# #-}
unintern# (Symbol# ba#) = textualFromBA# ba#

-- | Version of `unintern` that works directly on an unlifted `Symbol#`
-- and returns the internal `ByteArray#
unintern## :: Symbol# -> ByteArray#
{-# INLINE unintern## #-}
unintern## (Symbol# ba#) = ba#

-- | Equality checking on unlifted symbols.
--
-- This takes only O(1) time, and is a simple pointer-equality check.
sameSymbol# :: Symbol# -> Symbol# -> Bool
{-# INLINE sameSymbol# #-}
sameSymbol# a b =
  case sameSymbol## a b of
    0# -> False
    _ -> True

-- | Version of `sameSymbol#` that returns the an `Int#`, an unlifted `Bool`
sameSymbol## :: Symbol# -> Symbol# -> Int#
{-# INLINE sameSymbol## #-}
sameSymbol## (Symbol# a) (Symbol# b) = sameByteArray# a b

-- | Hash an unlifted `Symbol#`
--
-- Takes O(1) and results in zero collissions, as `StableName`s are used.
hashSymbol# :: Symbol# -> Int
{-# INLINE hashSymbol# #-}
hashSymbol# sym# = I# (hashSymbol## sym#)

-- | Hash an unlifted `Symbol#`, returning an unlifted `Int#`
--
-- Takes O(1) and results in zero collissions, as `StableName`s are used.
hashSymbol## :: Symbol# -> Int#
{-# INLINE hashSymbol## #-}
hashSymbol## (Symbol# ba#) = Accursed.byteArrayStableNameHash## ba#

-- | Compare two unlifted symbols
--
-- The symbols are compared lexicographically using their UTF-8 representation,
-- so this takes linear time.
compareSymbol# :: Symbol# -> Symbol# -> Ordering
{-# INLINE compareSymbol# #-}
compareSymbol# (Symbol# a) (Symbol# b) = Accursed.utf8CompareByteArray# a b

textualToBA# :: (Textual str) => str -> ByteArray#
{-# INLINE textualToBA# #-}
textualToBA# !str =
  let !(SBS ba#) = Text.Short.toShortByteString (Textual.toShortText str)
   in ba#

textualToBAUnsafe# :: (Textual str) => str -> ByteArray#
{-# INLINE textualToBAUnsafe# #-}
textualToBAUnsafe# !str =
  let !(SBS ba#) = Text.Short.toShortByteString (Textual.toShortTextUnsafe str)
   in ba#

textualFromBA# :: (Textual str) => ByteArray# -> str
{-# INLINE textualFromBA# #-}
textualFromBA# ba# = Textual.fromShortText (Text.Short.Unsafe.fromShortByteStringUnsafe (SBS ba#))

-- | This `Data` instance follows the same implementation as the one for 
-- `Text` and `ShortText`: It pretends `Symbol` is a a collection holding a `[Char]`.
--
-- @since 1.0.1.0
instance Data.Data.Data Symbolize.Symbol where
  gfoldl f z txt = z (Symbolize.intern @String) `f` Symbolize.unintern @String txt
  toConstr _ = internConstr
  gunfold k z c = case Data.Data.constrIndex c of
    1 -> k (z (Symbolize.intern @String))
    _ -> error "gunfold"
  dataTypeOf _ = symbolDataType

symbolDataType :: Data.Data.DataType
symbolDataType = Data.Data.mkDataType "Symbolize.Symbol" [internConstr]

internConstr :: Data.Data.Constr
internConstr = Data.Data.mkConstr symbolDataType "intern" [] Data.Data.Prefix

-- | Uses the `ShortByteString` instance of `Textual` under the hood;
-- invalid UTF-8 is replaced by the Unicode replacement character.
--
--  @since 1.0.1.0
instance Data.Binary.Binary Symbol where
  put = Data.Binary.put . Symbolize.unintern @ShortByteString
  get = Symbolize.intern @ShortByteString <$> Data.Binary.get
