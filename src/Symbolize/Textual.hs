{-# LANGUAGE FlexibleInstances #-}
-- NOTE: FlexibleInstances is needed to support `String` instance :-(

module Symbolize.Textual (Textual (..)) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as ShortByteString
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Text.Encoding.Error
import qualified Data.Text.Lazy as LText
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ShortText

-- | Implemented by any String-like types.
-- The symbol table uses `ShortText` for its internal storage, so any type which can be converted to it
-- can be turned to/from a `Symbolize.Symbol`.
--
-- Instance should handle potential invalid UTF-8 by using the Unicode replacement character,
-- c.f. `Data.Text.Encoding.Error.lenientDecode`.
class Textual a where
  toShortText :: a -> ShortText
  fromShortText :: ShortText -> a

-- |
-- - O(0) conversion (a no-op)
instance Textual ShortText where
  toShortText = id
  {-# INLINE toShortText #-}
  fromShortText = id
  {-# INLINE fromShortText #-}

-- |
-- - O(1) conversion
instance Textual Text where
  toShortText = ShortText.fromText
  {-# INLINE toShortText #-}
  fromShortText = ShortText.toText
  {-# INLINE fromShortText #-}

-- |
-- - O(n) conversion
instance Textual String where
  toShortText = ShortText.fromString
  {-# INLINE toShortText #-}
  fromShortText = ShortText.toString
  {-# INLINE fromShortText #-}

-- |
-- - O(1) conversion
instance Textual LText.Text where
  toShortText = ShortText.fromText . LText.toStrict
  {-# INLINE toShortText #-}
  fromShortText = LText.fromStrict . ShortText.toText
  {-# INLINE fromShortText #-}

-- |
-- - toShortText: O(n). Turns invalid UTF-8 into the Unicode replacement character.
-- - fromShortText: O(0) no-op
instance Textual ShortByteString where
  toShortText byteString =
    byteString
      & ShortByteString.fromShort
      & Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode
      & ShortText.fromText
  {-# INLINE toShortText #-}

  fromShortText = ShortText.toShortByteString
  {-# INLINE fromShortText #-}

-- |
-- - toShortText: O(n). Turns invalid UTF-8 into the Unicode replacement character.
-- - fromShortText: O(n).
instance Textual ByteString where
  toShortText byteString =
    byteString
      & Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode
      & ShortText.fromText
  {-# INLINE toShortText #-}

  fromShortText = ShortText.toByteString
  {-# INLINE fromShortText #-}
