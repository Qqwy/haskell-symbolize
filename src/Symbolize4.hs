{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
module Symbolize4 where
import Prelude hiding (lookup)
import Data.Function ((&))
import Data.Primitive.ByteArray (ByteArray#)
import GHC.Exts (sameByteArray#, StablePtr#,  Weak#, eqStablePtr#, isTrue#, makeStablePtr#, mkWeak#, Levity, TYPE, RuntimeRep(BoxedRep), RealWorld, State#, makeStableName#, StableName#, eqStableName#, stableNameToInt#, Int(I#), deRefWeak#, MutVar#)
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

import Symbolize.Textual (Textual)
import Symbolize.Textual qualified as Textual


newtype Symbol# = Symbol# ByteArray#
newtype WeakSymbol# = WeakSymbol# (Weak# Symbol#)

data Symbol where
    Symbol :: {-# UNPACK #-} !Symbol# -> Symbol

instance Eq Symbol where
    (Symbol (Symbol# sym1#)) == (Symbol (Symbol# sym2#)) =
        case sameByteArray# sym1# sym2# of
            0# -> False
            _ -> True

instance Show Symbol where
    show = show . symbolToShortText

instance Ord Symbol where
    a `compare` b = (symbolToShortText a) `compare` (symbolToShortText b)

data WeakSymbol where
    WeakSymbol :: {-# UNPACK #-} !WeakSymbol# -> WeakSymbol


newtype SymbolTable = SymbolTable {unSymbolTable :: (Set Symbol)}

newtype GlobalSymbolTable = GlobalSymbolTable {symbolTableRef :: Weak (IORef SymbolTable)}

instance Show GlobalSymbolTable where
    -- SAFETY: We're only reading, and do not care about performance here.
    show gsymtab = System.IO.Unsafe.unsafePerformIO $ do
        ioref <- fromMaybe defaultSymbolTable <$> Weak.deRefWeak (symbolTableRef gsymtab)
        SymbolTable symtab <- IORef.readIORef ioref
        -- set <- fromMaybe mempty <$> (Weak.deRefWeak symtab)
        -- let contents = Set.toList set
        let contents = Set.toList symtab
        pure $ "GlobalSymbolTable { contents = " <> show contents <> " }"


globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable = pure globalSymbolTable'

globalSymbolTable' :: GlobalSymbolTable
{-# NOINLINE globalSymbolTable' #-}
globalSymbolTable' = System.IO.Unsafe.unsafePerformIO $ do
    -- let !set = mempty
    -- !weak <- Weak.mkWeakPtr set Nothing
    let !ref = defaultSymbolTable
    !weak <- IORef.mkWeakIORef ref (pure ())
    pure (GlobalSymbolTable weak)

defaultSymbolTable = unsafePerformIO $ IORef.newIORef (SymbolTable mempty)

withSymbolTable :: SymbolTable -> (Set Symbol -> IO (Set Symbol, a)) -> IO (SymbolTable, a)
withSymbolTable (SymbolTable set) fun = do
    -- set <- fromMaybe mempty <$> Weak.deRefWeak weak
    (set', a) <- fun set
    -- weak' <- Weak.mkWeakPtr set' Nothing
    -- pure (SymbolTable weak', a)
    pure (SymbolTable set', a)


symbolHash :: Symbol -> Int
symbolHash sym = withSymbolStableName sym (\sname# -> I# (stableNameToInt# sname#))

-- symbolEq :: Symbol -> Symbol -> Bool
-- symbolEq sym1 sym2 = 
--     withSymbolStableName sym1 $ \sname1# -> 
--         withSymbolStableName sym2 $ \sname2# ->
--             case sname1# `eqStableName#` sname2# of
--                 0# -> False
--                 _ -> True


withSymbolStableName :: Symbol -> (StableName# Symbol# -> r) -> r
withSymbolStableName (Symbol sym#) fun = 
    unsafePerformIO $ primitive $ \s1 ->
        case makeStableName# sym# s1 of
            (# s2, sname# #) -> (# s2, fun sname# #)

shortTextToSymbol :: ShortText -> Symbol
shortTextToSymbol !text = 
    let !(SBS ba#) = Text.Short.toShortByteString text
    in Symbol (Symbol# ba#)

intern :: Textual str => str -> Symbol
intern !str =
    let !newSymbol = shortTextToSymbol (Textual.toShortText str) in
    unsafePerformIO $ do
        gsymtab <- globalSymbolTable
        ioref <- fromMaybe defaultSymbolTable <$> Weak.deRefWeak (symbolTableRef gsymtab)
        symtab <- IORef.readIORef ioref
        case lookupCritical newSymbol symtab of
            Just existingSymbol -> pure existingSymbol
            Nothing ->
                IORef.atomicModifyIORef' ioref $ \symtab ->
                    case lookupCritical newSymbol symtab of
                        Just existingSymbol -> (symtab, existingSymbol)
                        Nothing ->
                            unsafePerformIO $ do
                                withSymbolTable symtab $ \set -> do
                                    -- let !weak = (unSymbolTable symtab)
                                    -- set <- fromMaybe mempty <$> deRefWeak weak
                                    let !set' = Set.insert newSymbol set
                                    -- symtab' <- SymbolTable <$> mkWeakPtr set' Nothing
                                    -- putStrLn $ "Inserted " <> show newSymbol <> "Into symbol table " <> show symtab'
                                    addSymbolFinalizer newSymbol gsymtab (finalizer newSymbol)
                                    pure (set', newSymbol)

lookupCritical newSymbol (SymbolTable set) = 
    -- case unsafePerformIO (Weak.deRefWeak weak) of
    --     Nothing -> Nothing
    --     Just set ->
            (flip Set.elemAt set <$> Set.lookupIndex newSymbol set)

addSymbolFinalizer :: Symbol -> GlobalSymbolTable -> IO () -> IO ()
addSymbolFinalizer !symbol@(Symbol sym#) (GlobalSymbolTable (Weak gweak#)) !(IO finalizer#) = do
    primitive $ \s1 ->
        case mkWeak# sym# gweak# finalizer# s1 of
            (# s2, _ #) -> (# s2, () #)

finalizer :: Symbol -> IO ()
finalizer symbol = do
    putStrLn $ "Running finalizer for symbol" <> show symbol
    gsymtab <- globalSymbolTable
    ioref <- fromMaybe defaultSymbolTable <$> Weak.deRefWeak (symbolTableRef gsymtab)
    IORef.atomicModifyIORef' ioref $ \symtab -> unsafePerformIO $
        withSymbolTable symtab $ \set -> 
            case Set.lookupIndex symbol set of
                Nothing -> pure (set, ())
                Just idx -> 
                    let existingSymbol = Set.elemAt idx set in
                    case symbol == existingSymbol of
                        False -> pure (set, ())
                        True-> pure ((Set.deleteAt idx set), ())

unintern :: Textual str => Symbol -> str
unintern = Textual.fromShortText . symbolToShortText

symbolToShortText :: Symbol -> ShortText
symbolToShortText (Symbol (Symbol# ba#)) = Text.Short.Unsafe.fromShortByteStringUnsafe (SBS ba#)
