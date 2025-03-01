{-# LANGUAGE GHC2021 #-}
module Symbolize.NonEmptyWeakSymbol 
(NonEmptyWeakSymbol((:|)), 
toList, 
nonEmpty, 
singleton, 
cons
) where

import Symbolize.WeakSymbol (WeakSymbol)

-- | A monomorphised version of `Data.List.NonEmpty`
-- which allows its head `WeakSymbol` to be UNPACKed into it.
-- Since we expect collisions to be _very_ rare,
-- this skips the intermediate allocation almost always.
data NonEmptyWeakSymbol where
  (:|) :: {-# UNPACK #-} !WeakSymbol -> ![WeakSymbol] -> NonEmptyWeakSymbol

toList :: NonEmptyWeakSymbol -> [WeakSymbol]
{-# INLINE toList #-}
toList (a :| as) = a : as

nonEmpty :: [WeakSymbol] -> Maybe NonEmptyWeakSymbol
{-# INLINE nonEmpty #-}
nonEmpty [] = Nothing
nonEmpty (a : as) = Just (a :| as)

singleton :: WeakSymbol -> NonEmptyWeakSymbol
{-# INLINE singleton #-}
singleton a = a :| []

cons :: WeakSymbol -> NonEmptyWeakSymbol -> NonEmptyWeakSymbol
{-# INLINE cons #-}
cons y (x :| xs) = y :| (x : xs)
