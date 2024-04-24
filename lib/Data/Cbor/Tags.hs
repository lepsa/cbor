{-# LANGUAGE OverloadedLists #-}

module Data.Cbor.Tags where

import Data.Cbor
import Data.ByteString
import qualified Data.ByteString as B
import Numeric.Natural
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import Control.Applicative
import Data.Time.Clock.System (systemToUTCTime, SystemTime (..), utcToSystemTime)
import Data.Cbor.Decoder
import Data.Ratio
import Data.Cbor.Util
import Data.Parser
import Data.Cbor.Parser
import Data.Cbor.Serialiser (encode)
import qualified Data.Text.Lazy as TL
import Text.URI
import Data.Text (Text)
import qualified Data.ByteString.Base64.URL as B64U
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T

--
-- RFC8949 Section 3.4
--

-- Section 3.4.1, Datetime strings
toDateTime :: Cbor -> Maybe (Either ZonedTime UTCTime)
toDateTime (CTag 0 (CText t)) =
  let u = fmap Right . iso8601ParseM $ T.unpack t
      z = fmap Left . iso8601ParseM $ T.unpack t
  in u <|> z
toDateTime _ = Nothing

fromDateTime :: Either ZonedTime UTCTime -> Cbor
fromDateTime = CTag 0 . CText . T.pack . either iso8601Show iso8601Show

-- Section 3.4.2 Datetime epochs
-- Assumes that negative values are also handled as a simple seconds count before
-- the epoch, but the RFC specifically calls out that this should be application specific.
toUnixEpoch :: Cbor -> Maybe UTCTime
toUnixEpoch (CTag 1 c) = do
  diff <- decodedToMaybe (withInteger (pure . fromIntegral) c)
      <|> decodedToMaybe (withFloating (pure . realToFrac) c)
  pure $ addUTCTime diff $ systemToUTCTime $ MkSystemTime 0 0
toUnixEpoch _ = Nothing

fromUnixEpoch :: UTCTime -> Maybe Cbor
fromUnixEpoch u =
  let t = utcToSystemTime u
      secs = systemSeconds t
      nSecs = systemNanoseconds t
  in if nSecs == 0
  then -- We can use a simple integer count
    CTag 1 <$>
      if secs >= 0
      then pure $ CUnsigned $ fromIntegral secs
      else CNegative <$> toNegative (fromIntegral secs)
  else -- We have to use a fractional value
    pure $ CTag 1 $ CDouble $
      fromRational $ ((nanoScale * fromIntegral secs) + fromIntegral nSecs) % nanoScale
  where
    nanoScale :: Integer
    nanoScale = 1_000_000_000

-- Section 3.4.3 BigNums
-- Since all whole numbers are stored as unsigned ints when going over the wire
-- and negatives are a semantic applied by the major types, we can use unsigned
-- values when building up from the raw bytes and apply the negative semantics
-- when they are needed.
fromNetworkOrder :: ByteString -> Natural
fromNetworkOrder = go 0
  where
    go :: Natural -> ByteString -> Natural
    go i bs = case B.uncons bs of
      Nothing -> i
      Just (b, bs') -> go (fromIntegral b + (i * 256)) bs'

toNetworkOrder :: Natural -> ByteString
toNetworkOrder = go []
  where
    go :: ByteString -> Natural -> ByteString
    go bs 0 = bs
    go bs x =
      let w = rem x 256
      in go (B.cons (fromIntegral w) bs) (div x 256)

fromBigNum :: Cbor -> Maybe Integer
fromBigNum (CTag 2 (CByteString b)) = pure $ fromIntegral $ fromNetworkOrder b -- Positive bignums
fromBigNum (CTag 3 (CByteString b)) = pure $ negate (fromIntegral (fromNetworkOrder b)) - 1 -- Negative bignums
fromBigNum _ = Nothing

toBigNum :: Integer -> Maybe Cbor
toBigNum n | n >=  bounds64Bit = pure . CTag 2 . CByteString . toNetworkOrder $ fromIntegral n
           | n <= -bounds64Bit = pure . CTag 3 . CByteString . toNetworkOrder $ fromIntegral $ negate $ n + 1
           | otherwise = Nothing

-- Section 3.4.4 Decimal fractions and big floats
fromDecimalFrac :: Cbor -> Maybe Rational
fromDecimalFrac (CTag 4 (CArray [e, m])) = do
  e' <- decodedToMaybe $ withInteger pure e
  m' <- decodedToMaybe (withInteger pure m) <|> fromBigNum m
  pure $ fromIntegral m' * (10 ^ e')
fromDecimalFrac _ = Nothing

fromBigFloat :: Cbor -> Maybe Rational
fromBigFloat (CTag 5 (CArray [e, m])) = do
  e' <- decodedToMaybe $ withInteger pure e
  m' <- decodedToMaybe (withInteger pure m) <|> fromBigNum m
  pure $ fromIntegral m' * (2 ^ e')
fromBigFloat _ = Nothing

-- TODO
-- toDecimalFrac :: Rational -> Cbor
-- toDecimalFrac (x % y) =
--   let
--     x' = x*y
--     e = floor $ logBase 10 x'
-- toBigFloat :: Rational -> Cbor
-- toBigFloat = _

-- Section 3.4.5 Content Hints

-- Encoded CBOR
fromEncodedCbor :: Cbor -> Maybe Cbor
fromEncodedCbor (CTag 24 c) =
  let f = either (Left . Error . show) (pure . snd) . runParser cbor
  in decodedToMaybe (withByteString f c)
    <|> decodedToMaybe (withByteStringStreaming (f . B.toStrict) c)
fromEncodedCbor _ = Nothing

toEncodedCbor :: Cbor -> Maybe Cbor
toEncodedCbor = either (const Nothing) (pure . CTag 24 . CByteString) . encode

-- Expected later encodings
fromExpectedBase64Url :: Cbor -> Maybe Cbor
fromExpectedBase64Url (CTag 21 c) = pure c
fromExpectedBase64Url _ = Nothing

fromExpectedBase64 :: Cbor -> Maybe Cbor
fromExpectedBase64 (CTag 22 c) = pure c
fromExpectedBase64 _ = Nothing

fromExpectedBase16 :: Cbor -> Maybe Cbor
fromExpectedBase16 (CTag 23 c) = pure c
fromExpectedBase16 _ = Nothing

toExpectedBase64Url :: Cbor -> Cbor
toExpectedBase64Url = CTag 21

toExpectedBase64 :: Cbor -> Cbor
toExpectedBase64 = CTag 22

toExpectedBase16 :: Cbor -> Cbor
toExpectedBase16 = CTag 23

-- Encoded Text
fromEncodedURI :: Cbor -> Maybe URI
fromEncodedURI (CTag 32 c) =
  let f = maybe (Left $ Error "Could not parse URI") pure . mkURI
  in decodedToMaybe (withText f c) <|> decodedToMaybe (withTextStreaming (f . TL.toStrict) c)
fromEncodedURI _ = Nothing

toEncodedURI :: URI -> Cbor
toEncodedURI = CTag 32 . CText . T.pack . renderStr

fromBase64Url :: Cbor -> Maybe ByteString
fromBase64Url (CTag 33 c) =
  let f = either (Left . Error) pure . B64U.decode . encodeUtf8
  in decodedToMaybe (withText f c) <|> decodedToMaybe (withTextStreaming (f . TL.toStrict) c)
fromBase64Url _ = Nothing

toBase64Url :: ByteString -> Cbor
toBase64Url = CTag 33 . CText . T.decodeUtf8 . B64U.encode

fromBase64 :: Cbor -> Maybe ByteString
fromBase64 (CTag 34 c) =
  let f = either (Left . Error) pure . B64.decode . encodeUtf8
  in decodedToMaybe (withText f c) <|> decodedToMaybe (withTextStreaming (f . TL.toStrict) c)
fromBase64 _ = Nothing

toBase64 :: ByteString -> Cbor
toBase64 = CTag 34 . CText . T.decodeUtf8 . B64.encode

fromMIME :: Cbor -> Maybe Text
fromMIME (CTag 36 c) =
  decodedToMaybe (withText pure c) <|> decodedToMaybe (withTextStreaming (pure . TL.toStrict) c)
fromMIME _ = Nothing

toMIME :: Text -> Cbor
toMIME = CTag 36 . CText

fromSelfDescribing :: Cbor -> Maybe Cbor
fromSelfDescribing (CTag 55799 c) = pure c
fromSelfDescribing _ = Nothing

toSelfDescribing :: Cbor -> Cbor
toSelfDescribing = CTag 55799