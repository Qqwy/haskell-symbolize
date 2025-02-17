{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnboxedTuples #-}
module Symbolize6
( -- * Symbol
Symbol(..)
, intern
, unintern
, lookup
-- Introspection & Metrics
, SymbolTable.GlobalSymbolTable
, SymbolTable.globalSymbolTable
-- * Unlifted interface
, Symbol#
, intern#
, intern##
, unintern#
, unintern##
, sameSymbol#
, sameSymbol##
, hashSymbol#
, hashSymbol##
, compareSymbol#
)
 where
import Prelude hiding (lookup)
import Data.Function ((&))
import Control.Applicative ((<|>))
import Data.Array.Byte (ByteArray(ByteArray))
import GHC.Exts (ByteArray#, sameByteArray#, makeStableName#, stableNameToInt#, Int#, realWorld#)
import Symbolize.Textual (Textual)
import Symbolize.Textual qualified as Textual
import Symbolize.SymbolTable qualified as SymbolTable
import Data.ByteString.Short (ShortByteString(SBS))
import qualified Data.Text.Short as Text.Short
import qualified Symbolize.Accursed as Accursed
import qualified Data.Text.Short.Unsafe as Text.Short.Unsafe
import GHC.Int (Int(I#))
import Data.String (IsString(fromString))
import Text.Read (Lexeme (Ident), lexP, parens, prec, readListPrecDefault, Read(..))
import Text.Read qualified
import Control.DeepSeq (NFData (rnf))
import Data.Hashable (Hashable (hash, hashWithSalt))

newtype Symbol# = Symbol# ByteArray#

data Symbol where
    Symbol :: Symbol# -> Symbol

instance Eq Symbol where
    (Symbol a) == (Symbol b) = a `sameSymbol#` b

instance Ord Symbol where
    (Symbol a) `compare` (Symbol b) =  a `compareSymbol#` b

instance Show Symbol where
  showsPrec p symbol =
    let !str = unintern @String symbol
     in showParen (p > 10) $
          showString "Symbolize.intern " . shows str

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
  hash (Symbol s) = hashSymbol# s
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (Symbol s) = hashWithSalt salt (hashSymbol# s)

intern :: Textual str => str -> Symbol
{-# INLINE intern #-}
intern !str = Symbol (intern# str)

intern# :: Textual str => str -> Symbol#
{-# INLINE intern# #-}
intern# !str = intern## (textualToBA# str)

intern## :: ByteArray# -> Symbol#
{-# INLINE intern## #-}
intern## ba# = 
    let !(ByteArray ba2#) = Accursed.accursedUnutterablePerformIO (SymbolTable.insertGlobal ba#)
        in Symbol# ba2#

lookup :: Textual str => str -> IO (Maybe Symbol)
{-# INLINE lookup #-}
lookup !str = lookup# (textualToBA# str)

lookup# :: ByteArray# -> IO (Maybe Symbol)
{-# INLINE lookup# #-}
lookup# ba# =
    SymbolTable.lookupGlobal ba#
    & fmap (fmap (\(ByteArray ba2#) -> Symbol (Symbol# ba2#)))

unintern :: Textual str => Symbol -> str
{-# INLINE unintern #-}
unintern (Symbol sym#) = unintern# sym#

unintern# :: Textual str => Symbol# -> str
unintern# (Symbol# ba#) = textualFromBA# ba#

unintern## :: Symbol# -> ByteArray#
unintern## (Symbol# ba#) = ba#

sameSymbol# :: Symbol# -> Symbol# -> Bool
sameSymbol# a b = 
    case sameSymbol## a b of
        0# -> False
        _ -> True

sameSymbol## :: Symbol# -> Symbol# -> Int#
sameSymbol## (Symbol# a) (Symbol# b) = sameByteArray# a b

hashSymbol# :: Symbol# -> Int
hashSymbol# sym# = I# (hashSymbol## sym#)

hashSymbol## :: Symbol# -> Int#
hashSymbol## sym# =
    case makeStableName# sym# realWorld# of
        (# _, sname# #) -> stableNameToInt# sname#

compareSymbol# :: Symbol# -> Symbol# -> Ordering
compareSymbol# (Symbol# a) (Symbol# b) = Accursed.utf8CompareByteArray# a b 

textualToBA# :: Textual str => str -> ByteArray#
{-# INLINE textualToBA# #-}
textualToBA# !str = 
    let !(SBS ba#) = Text.Short.toShortByteString (Textual.toShortText str)
    in ba#

textualFromBA# :: Textual str => ByteArray# -> str
{-# INLINE textualFromBA# #-}
textualFromBA# ba# = Textual.fromShortText (Text.Short.Unsafe.fromShortByteStringUnsafe (SBS ba#))
