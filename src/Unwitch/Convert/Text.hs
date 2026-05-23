-- | Conversions from 'Data.Text.Text'.
module Unwitch.Convert.Text
  ( toLazyText
  , toString
  , fromString
  , toByteStringUtf8
  , toByteStringUtf16LE
  , toByteStringUtf16BE
  , toByteStringUtf32LE
  , toByteStringUtf32BE
  , toByteStringLatin1
  , toLazyByteStringUtf8
  , toByteStringBuilderUtf8
  , toTextBuilder
  , toShortByteStringUtf8
#ifdef __GLASGOW_HASKELL__
  , toPosixString
  , toWindowsString
  , toOsString
#endif
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as TLB

#ifdef __GLASGOW_HASKELL__
import Data.Coerce (coerce)
import System.OsString qualified as OS
import System.OsString.Internal.Types qualified as OSIT
import System.OsString.Posix qualified as OSP
#endif

toLazyText :: Text -> LT.Text
#ifdef __GLASGOW_HASKELL__
toLazyText = LT.fromStrict
#else
toLazyText = LT.toLazy
#endif

toString :: Text -> String
toString = T.unpack

fromString :: String -> Text
fromString = T.pack

toByteStringUtf8 :: Text -> ByteString
toByteStringUtf8 = TE.encodeUtf8

toByteStringUtf16LE :: Text -> ByteString
toByteStringUtf16LE = TE.encodeUtf16LE

toByteStringUtf16BE :: Text -> ByteString
toByteStringUtf16BE = TE.encodeUtf16BE

toByteStringUtf32LE :: Text -> ByteString
toByteStringUtf32LE = TE.encodeUtf32LE

toByteStringUtf32BE :: Text -> ByteString
toByteStringUtf32BE = TE.encodeUtf32BE

-- | Returns 'Nothing' if any character exceeds @\xFF@.
toByteStringLatin1 :: Text -> Maybe ByteString
toByteStringLatin1 t = if all isLatin1 str
  then Just $ BSC8.pack str
  else Nothing
  where str = T.unpack t

isLatin1 :: Char -> Bool
isLatin1 c = c <= '\xFF'

-- | Encode as UTF-8 lazy 'LBS.ByteString'.
toLazyByteStringUtf8 :: Text -> LBS.ByteString
toLazyByteStringUtf8 = LBS.fromStrict . TE.encodeUtf8

-- | Encode as UTF-8 'BB.Builder'.
toByteStringBuilderUtf8 :: Text -> BB.Builder
toByteStringBuilderUtf8 = TE.encodeUtf8Builder

-- | Convert to a lazy 'TLB.Builder'.
toTextBuilder :: Text -> TLB.Builder
toTextBuilder = TLB.fromText

-- | Encode as UTF-8 'ShortByteString'.
toShortByteStringUtf8 :: Text -> ShortByteString
toShortByteStringUtf8 = SBS.toShort . TE.encodeUtf8

#ifdef __GLASGOW_HASKELL__
-- | Encode as UTF-8 'OSP.PosixString'.
toPosixString :: Text -> OSP.PosixString
toPosixString = coerce . SBS.toShort . TE.encodeUtf8

-- | Encode as UTF-16 LE 'OSIT.WindowsString'.
toWindowsString :: Text -> OSIT.WindowsString
toWindowsString = coerce . SBS.toShort . TE.encodeUtf16LE

-- | Encode to the platform's native 'OS.OsString'.
-- UTF-16 LE on Windows, UTF-8 on POSIX.
toOsString :: Text -> OS.OsString
toOsString = case OS.coercionToPlatformTypes of
  Left{}  -> coerce . toWindowsString
  Right{} -> coerce . toPosixString
#endif
