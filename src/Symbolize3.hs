{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
module Symbolize3 where

import Prelude hiding (lookup)
import Data.Function ((&))
import Data.Primitive.ByteArray (ByteArray#)
import GHC.Exts (StablePtr#,  Weak#, eqStablePtr#, isTrue#, makeStablePtr#, mkWeak#, Levity, TYPE, RuntimeRep(BoxedRep), RealWorld, State#, makeStableName#, StableName#, eqStableName#, stableNameToInt#, Int(I#), deRefWeak#)
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
import Control.Monad.Primitive (PrimMonad(primitive))
import GHC.IO (IO(IO), unsafePerformIO)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

import Symbolize.Textual (Textual)
import Symbolize.Textual qualified as Textual

newtype Symbol# = Symbol# (StableName# ByteArray#)

data Symbol where
    Symbol :: {-# UNPACK #-} !Symbol# -> Symbol

instance Eq Symbol where
    (Symbol (Symbol# a)) == (Symbol (Symbol# b)) = isTrue# (a `eqStableName#` b)

instance Show Symbol where
    show = show . unintern

instance Ord Symbol where
    compare a b = compare (unintern a) (unintern b)

insert :: SymbolTable -> ShortText -> IO (SymbolTable, Symbol)
insert symtab !text = do
    symbol <- shortTextToSymbol text
    let symHash = symbolHash symbol
    weak <- makeWeakSymbol symbol (finalizer text symHash)
    let textToPtr' = Map.insert text weak (textToPtr symtab)
    let ptrToText' = IntMap.insert symHash text (ptrToText symtab)
    let symtab = SymbolTable {textToPtr = textToPtr', ptrToText =  ptrToText'}
    
    pure (symtab, symbol)

finalizer :: ShortText -> Int -> IO ()
finalizer !text !symHash = do
    -- putStrLn $ "finalizer: Deleting key" <> show text
    IORef.atomicModifyIORef' (symbolTableRef globalSymbolTable') $ \symtab ->
        let textToPtr' = Map.delete text (textToPtr symtab)
            ptrToText' = IntMap.delete symHash (ptrToText symtab)
        in
        (SymbolTable {textToPtr = textToPtr', ptrToText = ptrToText'}, ())


lookup :: Textual str => str -> IO (Maybe Symbol)
lookup !str = do
    let !text = Textual.toShortText str
    symtab <- IORef.readIORef (symbolTableRef globalSymbolTable')
    lookupCritical symtab text

lookupCritical :: SymbolTable -> ShortText -> IO (Maybe Symbol)
lookupCritical !symtab !text = 
    case Map.lookup text (textToPtr symtab) of
        Nothing -> pure Nothing
        Just (WeakSymbol (WeakSymbol# weak#)) -> 
            primitive $ \s1 ->
                case deRefWeak# weak# s1 of
                    (# s2, flag, sname# #) -> 
                        case flag of
                            0# -> (# s2, Nothing #)
                            _  -> (# s2, Just (Symbol (Symbol# sname#)) #)

intern :: Textual str => str -> Symbol
intern !str =
    let text = Textual.toShortText str in
    unsafePerformIO $ do
        maybeSym <- lookup text
        case maybeSym of
            Just sym -> pure sym
            Nothing ->
                IORef.atomicModifyIORef' (symbolTableRef globalSymbolTable') $ \symtab ->
                    unsafePerformIO $ do
                        -- We re-check if the symbol exists, as another thread might have inserted it
                        -- in-between above check and aquiring the IORef
                        maybeSym' <- lookupCritical symtab text 
                        case maybeSym' of
                            Just sym -> pure (symtab, sym)
                            Nothing -> insert symtab text


newtype WeakSymbol# = WeakSymbol# (Weak# (StableName# ByteArray#))

data WeakSymbol where
    WeakSymbol :: {-# UNPACK #-} !WeakSymbol# -> WeakSymbol

unpackShortText :: ShortText -> ByteArray#
unpackShortText !text =
    let !(SBS ba#) = Text.Short.toShortByteString text
    in ba#


shortTextToSymbol :: ShortText -> IO Symbol
shortTextToSymbol !text = do 
    let ba# = unpackShortText text
    primitive $ \s1 ->
        case makeStableName# ba# s1 of
            (# s2, sname# #) ->
                (# s2, Symbol (Symbol# sname#) #)

makeWeakSymbol :: Symbol -> IO () -> IO WeakSymbol
makeWeakSymbol !(Symbol (Symbol# sname#)) (IO finalizer#) = 
    primitive $ \s1 ->
        case mkWeak# sname# sname# finalizer# s1 of
            (# s2, weak# #) ->
                (# s2, WeakSymbol (WeakSymbol# weak#) #)

    -- let ba# = unpackShortText text
    -- sptr# <- makeStablePtr# ba#
    -- weak# <- mkWeak# sptr# ba# finalizer#
    -- let weak = Weak weak#
    -- pure weak

unintern :: Symbol -> ShortText
unintern !symbol = unsafePerformIO $ do
    symtab <- IORef.readIORef (symbolTableRef globalSymbolTable')
    let text = (ptrToText symtab) IntMap.! (symbolHash symbol)
    pure text


symbolHash :: Symbol -> Int
symbolHash (Symbol (Symbol# sname#)) = I# (stableNameToInt# sname#)
-- primitive' :: forall {l :: Levity} (a :: TYPE (BoxedRep l)). (State# RealWorld -> (# State# RealWorld, a #)) -> IO a
-- primitive' = IO

data SymbolTable = SymbolTable {
  -- Look up StableName given a text
  -- Not a hashmap to be collision-resistant (HashDoS resistant)
  textToPtr :: Map ShortText WeakSymbol,
  -- Look up text given a StableName, using `hashStableName` to make an int.
  -- We don't store StableNames themselves as keys as that would prevent them
  -- ever being GC'd
  ptrToText :: IntMap ShortText
}


newtype GlobalSymbolTable = GlobalSymbolTable {symbolTableRef :: IORef SymbolTable}

globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable = pure globalSymbolTable'

globalSymbolTable' :: GlobalSymbolTable
globalSymbolTable' = System.IO.Unsafe.unsafePerformIO $ do
    ref <- IORef.newIORef (SymbolTable mempty mempty)
    pure (GlobalSymbolTable ref)

globalSymbolTableSize :: IO Word
globalSymbolTableSize = do
    table <- globalSymbolTable
    (SymbolTable _ ptrToText) <- IORef.readIORef (symbolTableRef table)
    let size = fromIntegral $ IntMap.size ptrToText
    pure size


instance Show GlobalSymbolTable where
    -- SAFETY: We're only reading, and do not care about performance here.
    show table = System.IO.Unsafe.unsafePerformIO $ do
        SymbolTable _ ptrToText <- IORef.readIORef (symbolTableRef table)
        let contents = IntMap.toList ptrToText
        pure $ "GlobalSymbolTable { contents = " <> show contents <> " }"
