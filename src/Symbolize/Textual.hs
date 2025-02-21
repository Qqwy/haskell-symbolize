{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}

-- NOTE: FlexibleInstances is needed to support `String` instance :-(

module Symbolize.Textual (Textual (..)) where

import Data.Array.Byte (ByteArray (ByteArray))
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as ShortByteString
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Text.Encoding.Error
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Text.Builder
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ShortText
import qualified Data.Text.Short.Unsafe as ShortText.Unsafe

-- | Implemented by any String-like types.
-- The symbol table uses `ShortText` for its internal storage, so any type which can be converted to it
-- can be turned to/from a `Symbolize.Symbol`.
--
-- Instance should handle potential invalid UTF-8 by using the Unicode replacement character,
-- c.f. `Data.Text.Encoding.Error.lenientDecode`.
class Textual a where
  -- | Turns this text value into a `ShortText`, performing UTF-8 checking if necessary
  toShortText :: a -> ShortText

  -- | Turns this text value into a `ShortText`, potentially skipping UTF-8 checks
  toShortTextUnsafe :: a -> ShortText
  toShortTextUnsafe = toShortText

  -- | Turns a `ShortText` value into a different textual value
  fromShortText :: ShortText -> a

-- |
-- - O(0) conversion (a no-op)
instance Textual ShortText where
  toShortText :: ShortText -> ShortText
  {-# INLINE toShortText #-}
  toShortText = id

  toShortTextUnsafe :: ShortText -> ShortText
  {-# INLINE toShortTextUnsafe #-}
  toShortTextUnsafe = id

  fromShortText :: ShortText -> ShortText
  {-# INLINE fromShortText #-}
  fromShortText = id

-- |
-- - O(1) conversion
instance Textual Text where
  toShortText :: Text -> ShortText
  {-# INLINE toShortText #-}
  toShortText = ShortText.fromText

  fromShortText :: ShortText -> Text
  {-# INLINE fromShortText #-}
  fromShortText = ShortText.toText

-- |
-- - O(n) conversion
instance Textual String where
  toShortText :: String -> ShortText
  {-# INLINE toShortText #-}
  toShortText = ShortText.fromString

  fromShortText :: ShortText -> String
  {-# INLINE fromShortText #-}
  fromShortText = ShortText.toString

-- |
-- - O(1) conversion
instance Textual LText.Text where
  toShortText :: LText.Text -> ShortText
  {-# INLINE toShortText #-}
  toShortText = ShortText.fromText . LText.toStrict

  fromShortText :: ShortText -> LText.Text
  {-# INLINE fromShortText #-}
  fromShortText = LText.fromStrict . ShortText.toText

-- |
-- - toShortText: O(n). Evaluates the entire builder.
-- - fromShortText: O(1)
instance Textual Text.Builder.Builder where
  toShortText :: Text.Builder.Builder -> ShortText
  {-# INLINE toShortText #-}
  toShortText = ShortText.fromText . LText.toStrict . Text.Builder.toLazyText

  fromShortText :: ShortText -> Text.Builder.Builder
  {-# INLINE fromShortText #-}
  fromShortText = Text.Builder.fromText . ShortText.toText

-- |
-- - toShortText: O(n). Turns invalid UTF-8 into the Unicode replacement character.
-- - fromShortText: O(0) no-op
instance Textual ShortByteString where
  toShortText :: ShortByteString -> ShortText
  {-# INLINE toShortText #-}
  toShortText byteString =
    byteString
      & ShortByteString.fromShort
      & Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode
      & ShortText.fromText

  toShortTextUnsafe :: ShortByteString -> ShortText
  {-# INLINE toShortTextUnsafe #-}
  toShortTextUnsafe = ShortText.Unsafe.fromShortByteStringUnsafe

  fromShortText :: ShortText -> ShortByteString
  {-# INLINE fromShortText #-}
  fromShortText = ShortText.toShortByteString

-- |
-- - toShortText: O(n). Turns invalid UTF-8 into the Unicode replacement character.
-- - fromShortText: O(n).
instance Textual ByteString where
  toShortText :: ByteString -> ShortText
  {-# INLINE toShortText #-}
  toShortText byteString =
    byteString
      & Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode
      & ShortText.fromText

  toShortTextUnsafe :: ByteString -> ShortText
  {-# INLINE toShortTextUnsafe #-}
  toShortTextUnsafe = toShortTextUnsafe . ShortByteString.toShort

  fromShortText :: ShortText -> ByteString
  {-# INLINE fromShortText #-}
  fromShortText = ShortText.toByteString

-- |
-- - toShortText: O(n). Turns invalid UTF-8 into the Unicode replacement character.
-- - fromShortText: O(0) no-op
instance Textual ByteArray where
  toShortText :: ByteArray -> ShortText
  {-# INLINE toShortText #-}
  toShortText (ByteArray ba) = toShortText (ShortByteString.SBS ba)

  toShortTextUnsafe :: ByteArray -> ShortText
  {-# INLINE toShortTextUnsafe #-}
  toShortTextUnsafe (ByteArray ba) = toShortTextUnsafe (ShortByteString.SBS ba)

  fromShortText :: ShortText -> ByteArray
  {-# INLINE fromShortText #-}
  fromShortText (fromShortText -> ShortByteString.SBS ba) = ByteArray ba
