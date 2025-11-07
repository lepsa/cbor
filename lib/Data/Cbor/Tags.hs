{-# LANGUAGE OverloadedLists #-}

module Data.Cbor.Tags where

import           Data.ByteString
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Base64.URL as B64U
import           Data.Cbor
import           Data.Cbor.Decoder
import           Data.Cbor.Util
import           Data.Foldable
import           Data.Int
import           Data.Ratio
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.Encoding         as T
import           Data.Time
import           Data.Time.Clock.System     (SystemTime (..), systemToUTCTime,
                                             utcToSystemTime)
import           Data.Time.Format.ISO8601
import           Numeric.Natural
import           Text.URI

--
-- RFC8949 Section 3.4
--

-- Section 3.4.1, Datetime strings
withDateTime :: Decoder (Either ZonedTime UTCTime) a
withDateTime f = withTagValue 0 $ \c -> flip withText c $ \t ->
    let u = maybe (Left $ UnexpectedValue "Expected a ZonedTime" c) (pure . Left) . iso8601ParseM $ T.unpack t
        z = maybe (Left $ UnexpectedValue "Expected a UTCTime" c) (pure . Right) . iso8601ParseM $ T.unpack t
    in u <> z >>= f

fromDateTime :: Either ZonedTime UTCTime -> Cbor
fromDateTime = CTag 0 . CText . T.pack . either iso8601Show iso8601Show

-- Section 3.4.2 Datetime epochs
-- Assumes that negative values are also handled as a simple seconds count before
-- the epoch, but the RFC specifically calls out that this should be application specific.
withUnixEpoch :: Decoder UTCTime a
withUnixEpoch f = withTagValue 1 $ \c -> do
  diff <- withIntegral (pure . fromIntegral) c <> withFloating (pure . realToFrac) c
  f . addUTCTime diff . systemToUTCTime $ MkSystemTime 0 0

fromUnixEpoch :: UTCTime -> Cbor
fromUnixEpoch u =
  let t = utcToSystemTime u
      secs = systemSeconds t
      nSecs = systemNanoseconds t
  in CTag 1 $ if nSecs == 0
  then -- We can use a simple integer count
    cInteger $ fromIntegral secs
  else -- We have to use a fractional value
    CDouble . fromRational $ ((nanoScale * fromIntegral secs) + fromIntegral nSecs) % nanoScale
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
      Nothing       -> i
      Just (b, bs') -> go (fromIntegral b + (i * 256)) bs'

toNetworkOrder :: Natural -> ByteString
toNetworkOrder = go []
  where
    go :: ByteString -> Natural -> ByteString
    go bs 0 = bs
    go bs x =
      let w = rem x 256
      in go (B.cons (fromIntegral w) bs) (div x 256)

withBigNum :: Decoder Integer a
withBigNum f c = flip withTag c $ \(n, c') -> case n of
  2 -> flip withByteString c' $ f . fromIntegral . fromNetworkOrder
  3 -> flip withByteString c' $ \b -> f $ negate (fromIntegral (fromNetworkOrder b)) - 1
  _ -> Left $ UnexpectedValue "Expected a tag value of 2 or 3" c

-- TODO: Split the output, currently this mixes tagged big nums and 64 bit values
toBigNum :: Integer -> Cbor
toBigNum n | n >=  bounds64Bit = CTag 2 . CByteString . toNetworkOrder $ fromIntegral n
           | n <= -bounds64Bit = CTag 3 . CByteString . toNetworkOrder $ fromIntegral $ negate $ n + 1
           | otherwise = cInteger (fromIntegral n)

-- Section 3.4.4 Decimal fractions and big floats
withDecimalFrac :: Decoder Rational a
withDecimalFrac f = withTagValue 4 decodeTag
  where
  decodeTag c = withArray (decodeArray c) c
  decodeArray _ [e, m] = do
    e' <- withIntegral pure e
    m' <- withIntegral (pure . fromIntegral) m <> withBigNum pure m
    f $ fromInteger m' * (10 ^^ e')
  decodeArray c' _ = Left $ UnexpectedValue "Expected an array with exactly 2 values" c'

withBigFloat :: Decoder Rational a
withBigFloat f = withTagValue 5 decodeTag
  where
  decodeTag c = withArray (decodeArray c) c
  decodeArray _ [e, m] = do
    e' <- withIntegral pure e
    m' <- withIntegral (pure . fromIntegral) m <> withBigNum pure m
    f $ fromIntegral m' * (2 ^^ e')
  decodeArray c' _ = Left $ UnexpectedValue "Expected an array with exactly 2 values" c'

toDecimalFrac :: Integer -> Int64 -> Cbor
toDecimalFrac m e = CTag 4 (CArray [cInteger e, toBigNum m])

toBigFloat :: Integer -> Int64 -> Cbor
toBigFloat m e = CTag 5 (CArray [cInteger e, toBigNum m])

-- Section 3.4.5 Content Hints

-- Encoded CBOR
withEncodedCbor :: Decoder ByteString a
withEncodedCbor f = withTagValue 24 decodeTag
  where
  decodeTag c' = withByteString f c' <> withByteStringStreaming (f . fold) c'

toEncodedCbor :: ByteString -> Cbor
toEncodedCbor = CTag 24 . CByteString

-- Expected later encodings
withExpectedBase64Url :: Decoder Cbor a
withExpectedBase64Url = withTagValue 21

withExpectedBase64 :: Decoder Cbor a
withExpectedBase64 = withTagValue 22

withExpectedBase16 :: Decoder Cbor a
withExpectedBase16 = withTagValue 23

toExpectedBase64Url :: Cbor -> Cbor
toExpectedBase64Url = CTag 21

toExpectedBase64 :: Cbor -> Cbor
toExpectedBase64 = CTag 22

toExpectedBase16 :: Cbor -> Cbor
toExpectedBase16 = CTag 23

-- Encoded Text
withEncodedURI :: Decoder URI a
withEncodedURI f = withTagValue 32 $ \c ->
  let g = maybe (Left $ Error "Could not parse URI") f . mkURI
  in withText g c <> withTextStreaming (g . fold) c

toEncodedURI :: URI -> Cbor
toEncodedURI = CTag 32 . CText . T.pack . renderStr

withBase64Url :: Decoder ByteString a
withBase64Url f = withTagValue 33 $ \c ->
  let g = either (Left . Error) f . B64U.decode . encodeUtf8
  in withText g c <> withTextStreaming (g . fold) c

toBase64Url :: ByteString -> Cbor
toBase64Url = CTag 33 . CText . T.decodeUtf8 . B64U.encode

withBase64 :: Decoder ByteString a
withBase64 f = withTagValue 34 $ \c ->
  let g = either (Left . Error) f . B64.decode . encodeUtf8
  in withText g c <> withTextStreaming (g . fold) c

toBase64 :: ByteString -> Cbor
toBase64 = CTag 34 . CText . T.decodeUtf8 . B64.encode

withMIME :: Decoder Text a
withMIME f = withTagValue 36 $ \c ->
  withText f c <> withTextStreaming (f . fold) c

toMIME :: Text -> Cbor
toMIME = CTag 36 . CText

withMIMEByteString :: Decoder ByteString a
withMIMEByteString f = withTagValue 257 $ \c ->
  withByteString f c <> withByteStringStreaming (f . fold) c

toMIMEByteString :: ByteString -> Cbor
toMIMEByteString = CTag 257 . CByteString

-- 3.4.6 Self-Described CBOR

withSelfDescribing :: Decoder Cbor a
withSelfDescribing = withTagValue 55799

toSelfDescribing :: Cbor -> Cbor
toSelfDescribing = CTag 55799
