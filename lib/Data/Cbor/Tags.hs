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
import Data.Word
import Data.Ratio
import Data.Cbor.Util

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

fromUnixEpoch :: Either ZonedTime UTCTime -> Maybe Cbor
fromUnixEpoch e =
  let t = utcToSystemTime $ either zonedTimeToUTC id e
      secs = systemSeconds t
      nSecs = systemNanoseconds t
  in if nSecs == 0
  then -- We can use a simple integer count
    CTag 1 <$>
      if secs >= 0
      then
        let pos = fromIntegral secs
        -- Bounds check the our time diff can fit in 64 bits
        in if pos > fromIntegral (maxBound @Word64)
           then Nothing
           else pure $ CUnsigned pos
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

fromBigNum :: Cbor -> Maybe Integer
fromBigNum (CTag 2 (CByteString b)) = pure $ fromIntegral $ fromNetworkOrder b -- Positive bignums
fromBigNum (CTag 3 (CByteString b)) = pure $ negate (fromIntegral $ fromNetworkOrder b) - 1 -- Negative bignums
fromBigNum _ = Nothing

toBigNum :: Integer -> Maybe Cbor
toBigNum n | n >= bounds64Bit = pure $ CTag 2 $ CByteString $ toNetworkOrder $ fromIntegral n
           | n <= -bounds64Bit = pure $ CTag 3 $ CByteString $ toNetworkOrder $ fromIntegral $ negate n
           | otherwise = Nothing
  where
    toNetworkOrder :: Natural -> ByteString
    toNetworkOrder = go []
      where
        go :: ByteString -> Natural -> ByteString
        go bs 0 = bs
        go bs x =
          let w = rem n 256
          in go (B.cons (fromIntegral w) bs) (div x 256)

-- Section 3.4.4 Decimal fractions and big floats
fromDecimalFrac :: Cbor -> Maybe Rational
fromDecimalFrac (CTag 4 (CArray [e, m])) = do
  e' <- decodedToMaybe $ withInteger pure e
  m' <- decodedToMaybe (withInteger pure m) <|> fromBigNum m
  pure $ fromIntegral m' * (10 ^ e')
fromDecimalFrac (CTag 5 (CArray [e, m])) = do
  e' <- decodedToMaybe $ withInteger pure e
  m' <- decodedToMaybe (withInteger pure m) <|> fromBigNum m
  pure $ fromIntegral m' * (2 ^ e')
fromDecimalFrac _ = Nothing

-- toDecimalFrac :: Rational -> Cbor
-- toDecimalFrac = _