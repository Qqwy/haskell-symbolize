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
import GHC.Exts (StablePtr#,  Weak#, eqStablePtr#, isTrue#, makeStablePtr#, mkWeak#, Levity, TYPE, RuntimeRep(BoxedRep), RealWorld, State#, makeStableName#, StableName#, eqStableName#, stableNameToInt#, Int(I#), deRefWeak#, MutVar#)
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


newtype Symbol# = Symbol# (MutVar# RealWorld ByteArray#)
newtype WeakSymbol# = WeakSymbol# (Weak# Symbol#)

data Symbol where
    Symbol :: {-# UNPACK #-} !Symbol# -> Symbol

data WeakSymbol where
    WeakSymbol :: {-# UNPACK #-} WeakSymbol# -> WeakSymbol


newtype SymbolTable = SymbolTable (Map ShortText WeakSymbol)

newtype GlobalSymbolTable = GlobalSymbolTable {symbolTableRef :: IORef SymbolTable}

globalSymbolTable :: IO GlobalSymbolTable
globalSymbolTable = do
    ref <- IORef.newIORef (SymbolTable mempty)
    pure (GlobalSymbolTable ref)


symbolHash :: Symbol -> Int
symbolHash (Symbol (Symbol# mutvar#)) = 
    primitive $ \s1 ->
        case makeStableName# mutvar# s1 of
            (# s2, sname# #) ->
                (# s2, I# (stableNameToInt# sname#) #)
