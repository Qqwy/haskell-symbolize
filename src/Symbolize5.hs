{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
module Symbolize5 where
import Prelude hiding (lookup)
import Data.Function ((&))
import Data.Primitive.ByteArray (ByteArray#)
import GHC.Exts (newMutVar#, readMutVar#, sameMutVar#, sameByteArray#, StablePtr#,  Weak#, eqStablePtr#, isTrue#, makeStablePtr#, mkWeak#, Levity, TYPE, RuntimeRep(BoxedRep), RealWorld, State#, makeStableName#, StableName#, eqStableName#, stableNameToInt#, Int(I#), deRefWeak#, MutVar#)
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

newtype Symbol# = Symbol# (MutVar# RealWorld ByteArray#)
newtype WeakSymbol# = WeakSymbol# (Weak# Symbol#)

data Symbol where
    Symbol :: {-# UNPACK #-} !Symbol# -> Symbol

instance Show Symbol where
    show = show . symbolToShortText

instance Eq Symbol where
    (Symbol (Symbol# sym1#)) == (Symbol (Symbol# sym2#)) =
        case sameMutVar# sym1# sym2# of
            0# -> False
            _ -> True

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
symbolHash sym = withSymbolStableName sym (\sname# -> I# (stableNameToInt# sname#))

withSymbolStableName :: Symbol -> (StableName# Symbol# -> r) -> r
withSymbolStableName (Symbol sym#) fun = 
    unsafePerformIO $ primitive $ \s1 ->
        case makeStableName# sym# s1 of
            (# s2, sname# #) -> (# s2, fun sname# #)

lookupCritical !text (SymbolTable map) =
    case Map.lookup text map of
        Nothing -> Nothing
        Just weakSymbol -> 
            unsafePerformIO (deRefWeakSymbol weakSymbol)

intern !str = 
    let !text = Textual.toShortText str in
    unsafePerformIO $ do
        GlobalSymbolTable gsymtab <- globalSymbolTable
        symtab <- IORef.readIORef gsymtab
        case lookupCritical text symtab of
            Just existingSymbol -> pure existingSymbol
            Nothing ->
                IORef.atomicModifyIORef' gsymtab $ \symtab ->
                    case lookupCritical text symtab of
                        Just existingSymbol -> (symtab, existingSymbol)
                        Nothing -> unsafePerformIO $ do
                            newSymbol <- shortTextToNewSymbol text
                            weakSymbol <- mkWeakSymbol newSymbol (finalizer newSymbol)
                            let symtab' = SymbolTable $ Map.insert text weakSymbol $ unSymbolTable symtab
                            pure (symtab', newSymbol)


shortTextToNewSymbol :: ShortText -> IO Symbol
shortTextToNewSymbol !text = do
    let !(SBS ba#) = Text.Short.toShortByteString text
    primitive $ \s1 -> 
        case newMutVar# ba# s1 of
            (# s2, var# #) ->
                (# s2, Symbol (Symbol# var#) #)

mkWeakSymbol :: Symbol -> IO () -> IO WeakSymbol
mkWeakSymbol !symbol@(Symbol sym#) !(IO finalizer#) = do
    primitive $ \s1 ->
        case mkWeak# sym# sym# finalizer# s1 of
            (# s2, weak #) -> (# s2, WeakSymbol (WeakSymbol# weak) #)

finalizer :: Symbol -> IO ()
finalizer !symbol = do
    putStrLn $ "Running finalizer for symbol " <> show symbol
    let !text = (symbolToShortText symbol)
    (GlobalSymbolTable gsymtab) <- globalSymbolTable
    IORef.atomicModifyIORef' gsymtab $ \(SymbolTable symtab) -> 
        -- let symtab' = SymbolTable (Map.delete text (unSymbolTable symtab))
        -- in (symtab', ())
        unsafePerformIO $
            case Map.lookup text symtab of
                Nothing -> do
                    putStrLn $ "Lookup failed for " <> show symbol
                    pure (SymbolTable symtab, ())
                Just weak -> do
                    maybeSymbol <- deRefWeakSymbol weak
                    case maybeSymbol of
                        Nothing -> do 
                            let symtab' = (Map.delete text symtab)
                            putStrLn $ "DeRef failed for " <> show symbol
                            pure (SymbolTable symtab', ())
                        Just _ -> do 
                            putStrLn $ "Symbol was re-inserted before finalizer could acquire lock " <> show symbol
                            pure (SymbolTable symtab, ())

deRefWeakSymbol :: WeakSymbol -> IO (Maybe Symbol)
deRefWeakSymbol (WeakSymbol (WeakSymbol# w)) = primitive $ \s ->
   case deRefWeak# w s of
        (# s1, flag, p #) -> case flag of
                                0# -> (# s1, Nothing #)
                                _  -> (# s1, Just (Symbol p) #)

symbolToShortText :: Symbol -> ShortText
symbolToShortText (Symbol (Symbol# var#)) = unsafePerformIO $ primitive $ \s1 ->
    case readMutVar# var# s1 of
        (# s2, ba# #) ->
            (# s2, Text.Short.Unsafe.fromShortByteStringUnsafe (SBS ba#) #)
