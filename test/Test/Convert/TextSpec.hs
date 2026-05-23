module Test.Convert.TextSpec (spec) where

import Test.Hspec
import Data.Text qualified as T
import qualified Unwitch.Convert.Text as Text
import qualified Unwitch.Convert.LazyText as LazyText
import qualified Unwitch.Convert.ByteString as ByteString

#ifdef __GLASGOW_HASKELL__
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Short qualified as SBS
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as TLB
import qualified System.OsString as OS
import qualified System.OsString.Posix as OSP
#endif

spec :: Spec
spec = describe "Unwitch.Convert.Text" $ do

  describe "toLazyText / toText round-trip" $
    it "round-trips" $
      let t = T.pack "hello world"
      in LazyText.toText (Text.toLazyText t) `shouldBe` t

  describe "toString / fromString" $ do
    it "round-trips" $
      let s = "test string"
      in Text.toString (Text.fromString s) `shouldBe` s
    it "handles unicode" $
      let s = "\x00E9\x00FC\x00F1"
      in Text.toString (Text.fromString s) `shouldBe` s

  describe "toByteStringUtf8" $ do
    it "encodes ASCII" $
      Text.toByteStringUtf8 "hello" `shouldBe` "hello"
    it "round-trips with ByteString.toTextUtf8" $
      let t = T.pack "caf\x00E9"
      in ByteString.toTextUtf8 (Text.toByteStringUtf8 t) `shouldBe` Right t

  describe "toByteStringUtf16LE" $
    it "produces valid UTF-16LE output" $
      let t = T.pack "A"
          bs = Text.toByteStringUtf16LE t
      in ByteString.toTextUtf16LE bs `shouldBe` Right t

  describe "toByteStringUtf16BE" $
    it "produces valid UTF-16BE output" $
      let t = T.pack "B"
          bs = Text.toByteStringUtf16BE t
      in ByteString.toTextUtf16BE bs `shouldBe` Right t

  describe "toByteStringUtf32LE" $
    it "produces valid UTF-32LE output" $
      let t = T.pack "C"
          bs = Text.toByteStringUtf32LE t
      in ByteString.toTextUtf32LE bs `shouldBe` Right t

  describe "toByteStringUtf32BE" $
    it "produces valid UTF-32BE output" $
      let t = T.pack "D"
          bs = Text.toByteStringUtf32BE t
      in ByteString.toTextUtf32BE bs `shouldBe` Right t

  describe "toByteStringLatin1" $ do
    it "succeeds for ASCII text" $
      Text.toByteStringLatin1 "hello" `shouldSatisfy` \case
        Just _  -> True
        Nothing -> False
    it "succeeds for Latin1 range chars" $
      let t = T.pack "\x00E9\x00FC"
      in case Text.toByteStringLatin1 t of
           Just bs -> ByteString.toTextLatin1 bs `shouldBe` t
           Nothing -> expectationFailure "expected Just"
    it "fails for chars above 0xFF" $
      let t = T.pack "\x0100" -- Latin Extended-A
      in Text.toByteStringLatin1 t `shouldBe` Nothing

#ifdef __GLASGOW_HASKELL__
  describe "toLazyByteStringUtf8" $ do
    it "encodes ASCII" $
      Text.toLazyByteStringUtf8 "hello" `shouldBe` "hello"
    it "produces same bytes as toByteStringUtf8" $
      let t = T.pack "caf\x00E9"
      in LBS.toStrict (Text.toLazyByteStringUtf8 t) `shouldBe` Text.toByteStringUtf8 t

  describe "toByteStringBuilderUtf8" $
    it "produces same bytes as toByteStringUtf8" $
      let t = T.pack "caf\x00E9 \x1F600"
      in LBS.toStrict (BB.toLazyByteString (Text.toByteStringBuilderUtf8 t))
           `shouldBe` Text.toByteStringUtf8 t

  describe "toTextBuilder" $
    it "produces same text as input" $
      let t = T.pack "hello \x00E9\x1F600"
      in LT.toStrict (TLB.toLazyText (Text.toTextBuilder t)) `shouldBe` t

  describe "toShortByteStringUtf8" $
    it "produces same bytes as toByteStringUtf8" $
      let t = T.pack "caf\x00E9 \x1F600"
      in SBS.fromShort (Text.toShortByteStringUtf8 t) `shouldBe` Text.toByteStringUtf8 t

  describe "toPosixString" $
    it "matches encodeUtf from os-string" $
      let t = T.pack "caf\x00E9"
          expected = OSP.unsafeEncodeUtf (T.unpack t)
      in Text.toPosixString t `shouldBe` expected

  describe "toOsString" $
    it "matches encodeUtf from os-string" $
      let t = T.pack "hello/world.txt"
          expected = OS.unsafeEncodeUtf (T.unpack t)
      in Text.toOsString t `shouldBe` expected
#endif
