{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -ddump-cmm  #-}
module Symbolize5 where
import Prelude hiding (lookup)
import Data.Function ((&))
import Data.Primitive.ByteArray (ByteArray#)
import GHC.Exts (reallyUnsafePtrEquality#, UnliftedType, newMutVar#, readMutVar#, sameMutVar#, sameByteArray#, StablePtr#,  Weak#, eqStablePtr#, isTrue#, makeStablePtr#, mkWeak#, Levity, TYPE, RuntimeRep(BoxedRep), RealWorld, State#, makeStableName#, StableName#, eqStableName#, stableNameToInt#, Int(I#), deRefWeak#, MutVar#)
import Data.Text.Short (ShortText)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Weak (Weak(..))
import qualified System.Mem.Weak as Weak
import qualified System.IO.Unsafe
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.ByteString.Short (ShortByteString(SBS))
import qualified Data.Text.Short as Text.Short
import qualified Data.Text.Short.Unsafe as Text.Short.Unsafe
import Control.Monad.Primitive (PrimMonad(primitive))
import GHC.IO (IO(IO), unsafePerformIO)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable(..))
import Data.String (IsString(..))
import qualified Text.Read
import GHC.Read (Read (..))
import Text.Read (Lexeme (Ident), lexP, parens, prec, readListPrecDefault)
import Control.Applicative ((<|>))

import Symbolize.Textual (Textual)
import Symbolize.Textual qualified as Textual
import Symbolize.Accursed qualified

data Symbol# :: UnliftedType where 
    Symbol# :: ByteArray# -> Symbol#

newtype WeakSymbol# = WeakSymbol# (Weak# Symbol#)

data Symbol where
    Symbol :: {-# UNPACK #-} !Symbol# -> Symbol

instance Show Symbol where
    show symbol = "Symbolize5.intern " <> show (symbolToShortText symbol)

instance Eq Symbol where
    {-# INLINE (==) #-}
    (Symbol sym1#) == (Symbol sym2#) =
        case reallyUnsafePtrEquality# sym1# sym2# of
            0# -> False
            _ -> True

instance Ord Symbol where
    {-# INLINE compare #-}
    a `compare` b = (symbolToShortText a) `compare` (symbolToShortText b)

instance NFData Symbol where
    {-# INLINE rnf #-}
    rnf a = seq a ()

instance Hashable Symbol where
    {-# INLINE hash #-}
    hash symbol = symbolHash symbol
    hashWithSalt salt symbol = hashWithSalt salt (symbolHash symbol)

instance IsString Symbol where
    fromString = intern

instance Read Symbol where
  readListPrec = readListPrecDefault
  readPrec = parens $ prec 10 $ full <|> onlyString
    where
      onlyString = do
        str <- readPrec @String
        return $ intern str
      full = do
        Ident "Symbolize5" <- lexP
        Text.Read.Symbol "." <- lexP
        Ident "intern" <- lexP
        str <- readPrec @String
        return $ intern str

data WeakSymbol where
    WeakSymbol :: {-# UNPACK #-} !WeakSymbol# -> WeakSymbol

newtype SymbolTable = SymbolTable {unSymbolTable :: Map ShortText WeakSymbol}

newtype GlobalSymbolTable = GlobalSymbolTable {symbolTableRef :: (IORef SymbolTable)}

instance Show GlobalSymbolTable where
    -- SAFETY: We're only reading, and do not care about performance here.
    show table = System.IO.Unsafe.unsafePerformIO $ do
        SymbolTable symtab <- IORef.readIORef (symbolTableRef table)
        let contents = Map.keys symtab
        pure $ "GlobalSymbolTable { contents = " <> show contents <> " }"

globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable = pure globalSymbolTable'

globalSymbolTable' :: GlobalSymbolTable
{-# NOINLINE globalSymbolTable' #-}
globalSymbolTable' = System.IO.Unsafe.unsafePerformIO $ do
    let !set = mempty
    !ref <- IORef.newIORef (SymbolTable set)
    pure (GlobalSymbolTable ref)

symbolHash :: Symbol -> Int
symbolHash (Symbol sym#) = 
    Symbolize.Accursed.accursedUnutterablePerformIO $ primitive $ \s1 ->
        case makeStableName# sym# s1 of
            (# s2, sname# #) -> (# s2, I# (stableNameToInt#  sname#) #)

{-# INLINE lookupCritical #-}
lookupCritical :: ShortText -> SymbolTable -> Maybe Symbol
lookupCritical !text !(SymbolTable map) = Map.lookup text map >>= deRefWeakSymbol

intern :: Textual str => str -> Symbol
{-# INLINABLE intern #-}
intern !str =
    let !text = Textual.toShortText str in
    -- SAFETY: It is perfectly fine if floating/sharing happens
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

unintern :: Textual str => Symbol -> str
{-# INLINE unintern #-}
{-# SPECIALIZE unintern :: Symbol -> ShortText #-}
unintern symbol = Textual.fromShortText (symbolToShortText symbol)

shortTextToNewSymbol :: ShortText -> Symbol
shortTextToNewSymbol !text = 
    let !(SBS ba#) = Text.Short.toShortByteString text
    in Symbol (Symbol# ba#)

mkWeakSymbol :: Symbol -> IO () -> IO WeakSymbol
mkWeakSymbol !symbol@(Symbol sym#) !(IO finalizer#) = do
    primitive $ \s1 ->
        case mkWeak# sym# sym# finalizer# s1 of
            (# s2, weak #) -> (# s2, WeakSymbol (WeakSymbol# weak) #)

finalizer :: Symbol -> IO ()
finalizer !symbol = do
    -- putStrLn $ "Running finalizer for symbol " <> show symbol
    let !text = (symbolToShortText symbol)
    (GlobalSymbolTable gsymtab) <- globalSymbolTable
    IORef.atomicModifyIORef' gsymtab $ \(SymbolTable symtab) -> 
            case deRefWeakSymbol <$> Map.lookup text symtab of
                Just Nothing ->
                    let symtab' = (Map.delete text symtab) in
                    (SymbolTable symtab', ())
                _ -> 
                    -- Nothing to do if (a) it already was removed,
                    -- or (b) if it 'again exists' which means it was re-inserted
                    -- in-between GC and this finalizer being triggered
                    (SymbolTable symtab, ())

deRefWeakSymbol :: WeakSymbol -> (Maybe Symbol)
deRefWeakSymbol (WeakSymbol (WeakSymbol# w)) = Symbolize.Accursed.accursedUnutterablePerformIO $ primitive $ \s ->
   case deRefWeak# w s of
        (# s1, flag, p #) -> case flag of
                                0# -> (# s1, Nothing #)
                                _  -> (# s1, Just (Symbol p) #)

symbolToShortText :: Symbol -> ShortText
symbolToShortText (Symbol (Symbol# ba#)) = -- unsafePerformIO $ primitive $ \s1 ->
    Text.Short.Unsafe.fromShortByteStringUnsafe (SBS ba#)
