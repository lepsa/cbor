{-# LANGUAGE OverloadedStrings #-}

module Test.Roundtrip where

import           Data.ByteString
import           Data.ByteString.Builder
import           Data.Cbor
import           Data.Cbor.Decoder
import           Data.Cbor.Encoder
import           Data.Cbor.Parser
import           Data.Cbor.Tags
import           Data.Cbor.Util
import           Data.Int
import           Data.Parser             (Error, errorUnexpected, runParser)
import           Data.Scientific
import           Data.Time
import           Data.Time.Clock.System
import           Data.Word
import           Hedgehog
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range
import           Test.Util
import           Text.URI

genCbor:: Gen Cbor
genCbor = Gen.sized $ \(Range.Size size) -> Gen.choice
  [ CUnsigned   <$> Gen.word64      (Range.linear 0 maxBound)
  , CNegative   <$> Gen.word64      (Range.linear 0 maxBound)
  , CByteString <$> Gen.bytes       (Range.linear 0 size)
  , CText       <$> Gen.text        (Range.linear 0 size) Gen.unicodeAll
  , CArray      <$> Gen.list        (Range.linear 0 size) (halfSize genCbor)
  , CMap        <$> Gen.map         (Range.linear 0 size) ((,) <$> halfSize genCbor <*> halfSize genCbor)
  , CTag        <$> Gen.word64      (Range.linear 0 maxBound) <*> halfSize genCbor
  , CSimple     <$> Gen.filter (\x -> x < 23 || x > 31) (Gen.word8       (Range.linear 0 maxBound))
  , CHalf       <$> Gen.realFloat   (Range.linearFrac (negate $ convertSize size) (convertSize size))
  , CFloat      <$> Gen.realFloat   (Range.linearFrac (negate $ convertSize size) (convertSize size))
  , CDouble     <$> Gen.realFloat   (Range.linearFrac (negate $ convertSize size) (convertSize size))
  ]
  where
    convertSize :: Fractional a => Int -> a
    convertSize = fromRational . fromInteger . toInteger
    halfSize = Gen.scale scaleHalf

scaleHalf :: Size -> Size
scaleHalf s = s `div` 2

prop_roundtrip :: Property
prop_roundtrip = withTests 1000 . property $ do
  c <- forAll $ Gen.resize 100 genCbor
  tripping c serialise parse
  where
    serialise :: Cbor -> Either String Hex
    serialise = fmap (Hex . toStrict . toLazyByteString) . encode
    parse :: Either String Hex -> Either Error Cbor
    parse = either (Left . errorUnexpected) (fmap snd . runParser cbor . unHex)

prop_negativeMapping :: Property
prop_negativeMapping = withTests 1000 . property $ do
  v :: Word64 <- fmap fromIntegral $ forAll $ Gen.int64 $ Range.linear 0 maxBound
  tripping v fromNegative toNegative

newtype ZonedTimeEq = ZonedTimeEq {unZonedTimeEq :: ZonedTime}
  deriving (Show)

instance Eq ZonedTimeEq where
  ZonedTimeEq (ZonedTime tA tzA) == ZonedTimeEq (ZonedTime tB tzB) =
    tA == tB && tzA == tzB

prop_dateTime :: Property
prop_dateTime = withTests 1000 . property $ do
  unixTime <- forAll $ Gen.integral $ Range.linear 0 (fromIntegral $ maxBound @Word32)
  let utcTime = systemToUTCTime $ MkSystemTime unixTime 0
  time <-
    forAll $
      Gen.choice
        [ pure $ Right utcTime,
          do
            minutes <- Gen.int $ Range.linear (-(12 * 60)) (12 * 60)
            let tz = TimeZone minutes False ""
            pure $ Left $ ZonedTimeEq $ utcToZonedTime tz utcTime
        ]
  tripping time fromDateTime' toDateTime'
  pure ()
  where
    fromDateTime' = fromDateTime . either (Left . unZonedTimeEq) Right
    toDateTime' = withDateTime (pure . either (Left . ZonedTimeEq) Right)

newtype UTCTimeEpsilon = UTCTimeEpsilon
  { unUTCTimeEp :: UTCTime
  }
  deriving (Ord, Show)
instance Eq UTCTimeEpsilon where
  UTCTimeEpsilon a == UTCTimeEpsilon b =
    let aNeg = addUTCTime (-epsilon) a
        aPos = addUTCTime epsilon a
    in aNeg <= b && b <= aPos
    where
      epsilon = 000000000001

prop_unixEpoch :: Property
prop_unixEpoch = withTests 1000 . property $ do
  let epoch = systemToUTCTime $ MkSystemTime 0 0
  d <- forAll $ Gen.choice
    [ Left <$> Gen.integral @_ @Integer (Range.linear 0 (fromIntegral $ maxBound @Word32))
    , Right <$> Gen.double (Range.exponentialFloatFrom 0 (fromIntegral $ minBound @Word32) (fromIntegral $ maxBound @Word32))
    ]
  let difftime = either fromIntegral realToFrac d
      time = UTCTimeEpsilon $ addUTCTime difftime epoch
  tripping time fromUnixEpoch' toUnixEpoch'
  where
    fromUnixEpoch' :: UTCTimeEpsilon -> Cbor
    fromUnixEpoch' = fromUnixEpoch . unUTCTimeEp
    toUnixEpoch' :: Cbor -> Decoded UTCTimeEpsilon
    toUnixEpoch' = withUnixEpoch (pure . UTCTimeEpsilon)

prop_bigNum :: Property
prop_bigNum = withTests 1000 . property $ do
  n <- forAll $ Gen.choice
    [ Gen.integral $ Range.linear bounds64Bit (bounds64Bit*2)
    , Gen.integral $ Range.linear (negate $ bounds64Bit*2) (negate bounds64Bit - 1)
    ]
  tripping n toBigNum' fromBigNum'
  where
    toBigNum' :: Integer -> HexCbor
    toBigNum' = HexCbor . toBigNum
    fromBigNum' :: HexCbor -> Decoded Integer
    fromBigNum' = withBigNum pure . unHexCbor

prop_bigFrac :: Property
prop_bigFrac = withTests 1000 . property $ do
  m :: Integer <- forAll $ Gen.integral $ Range.linear (-10) 10
  e :: Int64 <- forAll $ Gen.integral $ Range.linear (-10) 10
  tripping (m, e) toDecimalFrac' fromDecimalFrac'
  where
    toDecimalFrac' :: (Integer, Int64) -> HexCbor
    toDecimalFrac' (m, e) = HexCbor $ toDecimalFrac m e
    fromDecimalFrac' :: HexCbor -> Maybe (Integer, Int64)
    fromDecimalFrac' (HexCbor (CTag 4 (CArray [e, m]))) = do
      e' <- either (const Nothing) pure $ withIntegral pure e
      m' <- either (const Nothing) pure $ withIntegral (pure . fromIntegral) m
      pure (m', e')
    fromDecimalFrac' _ = Nothing

prop_bigFracValues :: Property
prop_bigFracValues = withTests 1000 . property $ do
  m <- forAll $ Gen.integral $ Range.linear (-1000) 1000
  e <- forAll $ Gen.int64 $ Range.linear (-1000) 1000
  r <- evalEither $ withDecimalFrac pure $ toDecimalFrac m e
  unsafeFromRational r === Data.Scientific.scientific m (fromIntegral e)

prop_bigFloat :: Property
prop_bigFloat = withTests 1000 . property $ do
  m :: Integer <- forAll $ Gen.integral $ Range.linear (-10) 10
  e :: Int64 <- forAll $ Gen.integral $ Range.linear (-10) 10
  tripping (m, e) toBigFloat' fromBigFloat'
  where
    toBigFloat' :: (Integer, Int64) -> HexCbor
    toBigFloat' (m, e) = HexCbor $ toBigFloat m e
    fromBigFloat' :: HexCbor -> Maybe (Integer, Int64)
    fromBigFloat' (HexCbor (CTag 5 (CArray [e, m]))) = do
      e' <- either (const Nothing) pure $ withIntegral pure e
      m' <- either (const Nothing) pure $ withIntegral (pure . fromIntegral) m
      pure (m', e')
    fromBigFloat' _ = Nothing

prop_bigFloatValues :: Property
prop_bigFloatValues = withTests 1000 . property $ do
  m <- forAll $ Gen.integral $ Range.linear 0 100
  e <- forAll $ Gen.int64 $ Range.linear (-100) 100
  r <- evalEither $ withBigFloat pure $ toBigFloat m e
  r === (fromIntegral m * 2 ^^ e)

prop_encodedCbor :: Property
prop_encodedCbor = withTests 1000 . property $ do
  c <- forAll genCbor >>= (evalEither . encode) . CTag 24
  tripping (toStrict $ toLazyByteString c) toEncodedCbor' fromEncodedCbor'
  where
    toEncodedCbor' :: ByteString ->  HexCbor
    toEncodedCbor' = HexCbor . toEncodedCbor
    fromEncodedCbor' :: HexCbor -> Decoded ByteString
    fromEncodedCbor' = withEncodedCbor pure . unHexCbor

prop_expectedBase64Url :: Property
prop_expectedBase64Url = withTests 1000 . property $ do
  c <- CTag 21 <$> forAll genCbor
  tripping c toExpectedBase64Url $ withExpectedBase64Url pure

prop_expectedBase64 :: Property
prop_expectedBase64 = withTests 1000 . property $ do
  c <- CTag 22 <$> forAll genCbor
  tripping c toExpectedBase64 $ withExpectedBase64 pure

prop_expectedBase16 :: Property
prop_expectedBase16 = withTests 1000 . property $ do
  c <- CTag 23 <$> forAll genCbor
  tripping c toExpectedBase16 $ withExpectedBase16 pure

prop_encodedURI :: Property
prop_encodedURI = withTests 1000 . property $ do
  u <- forAll $ Gen.choice
    -- Example URIs from https://en.wikipedia.org/wiki/Uniform_Resource_Identifier
    [ Gen.mapMaybe mkURI $ pure "https://john.doe@www.example.com:123/forum/questions/?tag=networking&order=newest#top"
    , Gen.mapMaybe mkURI $ pure "ldap://[2001:db8::7]/c=GB?objectClass?one"
    , Gen.mapMaybe mkURI $ pure "mailto:John.Doe@example.com"
    , Gen.mapMaybe mkURI $ pure "news:comp.infosystems.www.servers.unix"
    , Gen.mapMaybe mkURI $ pure "tel:+1-816-555-1212"
    , Gen.mapMaybe mkURI $ pure "telnet://192.0.2.16:80/"
    , Gen.mapMaybe mkURI $ pure "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
    ]
  tripping u toEncodedURI (withEncodedURI pure)

prop_expectedMime :: Property
prop_expectedMime = withTests 1000 . property $ do
  t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  tripping t toMIME (withMIME pure)

prop_expectedMimeByteString :: Property
prop_expectedMimeByteString = withTests 1000 . property $ do
  t <- forAll $ Gen.bytes (Range.linear 0 100)
  tripping t toMIMEByteString (withMIMEByteString pure)
