{-# LANGUAGE FlexibleInstances #-} -- <- Needed to support `String` instance :-(
module Symbolize.Symbolizable (Symbolizable (..)) where

import Data.Function ((&))
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as ShortByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Text.Encoding.Error
import qualified Data.Text.Lazy as LText
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ShortText

class Symbolizable a where
  toShortText :: a -> ShortText
  fromShortText :: ShortText -> a

instance Symbolizable ShortText where
  toShortText = id
  fromShortText = id

instance Symbolizable Text where
  toShortText = ShortText.fromText
  fromShortText = ShortText.toText

instance Symbolizable [Char] where
  toShortText = ShortText.fromString
  fromShortText = ShortText.toString

instance Symbolizable LText.Text where
  toShortText = ShortText.fromText . LText.toStrict
  fromShortText = LText.fromStrict . ShortText.toText

instance Symbolizable ShortByteString where
  toShortText byteString =
    byteString
      & ShortByteString.fromShort
      & Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode
      & ShortText.fromText
  fromShortText = ShortText.toShortByteString

instance Symbolizable ByteString where
  toShortText byteString =
    byteString
      & Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode
      & ShortText.fromText
  fromShortText = ShortText.toByteString
