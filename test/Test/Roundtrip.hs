{-# LANGUAGE OverloadedStrings #-}

module Test.Roundtrip where

import           Data.Cbor
import           Data.Cbor.Decoder
import           Data.Cbor.Parser
import           Data.Cbor.Tags
import           Data.Cbor.Util
import           Data.Parser
import           Data.Time
import           Data.Time.Clock.System
import Data.ByteString
import Data.ByteString.Builder
import           Data.Word
import           Hedgehog
import Data.Cbor.Encoder
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
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
  v <- forAll $ Gen.word64 $ Range.linear minBound maxBound
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
    toDateTime' = fmap (either (Left . ZonedTimeEq) Right) . toDateTime

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
  tripping (pure time) fromUnixEpoch' toUnixEpoch'
  where
    fromUnixEpoch' :: Maybe UTCTimeEpsilon -> Maybe Cbor
    fromUnixEpoch' Nothing  = Nothing
    fromUnixEpoch' (Just t) = fromUnixEpoch $ unUTCTimeEp t
    toUnixEpoch' :: Maybe Cbor -> Maybe (Maybe UTCTimeEpsilon)
    toUnixEpoch' Nothing  = Nothing
    toUnixEpoch' (Just c) = pure $ UTCTimeEpsilon <$> toUnixEpoch c

prop_bigNum :: Property
prop_bigNum = withTests 1000 . property $ do
  n <- forAll $ Gen.choice
    [ Gen.integral $ Range.linear bounds64Bit (bounds64Bit*2)
    , Gen.integral $ Range.linear (negate $ bounds64Bit*2) (negate bounds64Bit - 1)
    ]
  tripping (pure n) toBigNum' fromBigNum'
  where
    toBigNum' :: Maybe Integer -> Maybe HexCbor
    toBigNum' Nothing  = Nothing
    toBigNum' (Just n) = HexCbor <$> toBigNum n
    fromBigNum' :: Maybe HexCbor -> Maybe (Maybe Integer)
    fromBigNum' Nothing  = Nothing
    fromBigNum' (Just n) = pure $ fromBigNum $ unHexCbor n

-- TODO
-- prop_bigFrac :: Property
-- prop_bigFrac = withTests 1000 . property $ do
--   r <- forAll $ Gen.integral $ Range.linear _ _

prop_encodedCbor :: Property
prop_encodedCbor = withTests 1000 . property $ do
  c <- CTag 24 <$> forAll genCbor
  tripping (pure c) toEncodedCbor' fromEncodedCbor'
  where
    toEncodedCbor' :: Maybe Cbor -> Maybe HexCbor
    toEncodedCbor' Nothing  = Nothing
    toEncodedCbor' (Just c) = HexCbor <$> toEncodedCbor c
    fromEncodedCbor' :: Maybe HexCbor -> Maybe (Maybe Cbor)
    fromEncodedCbor' Nothing  = Nothing
    fromEncodedCbor' (Just c) = pure $ fromEncodedCbor $ unHexCbor c

prop_expectedBase64Url :: Property
prop_expectedBase64Url = withTests 1000 . property $ do
  c <- CTag 21 <$> forAll genCbor
  tripping c toExpectedBase64Url fromExpectedBase64Url

prop_expectedBase64 :: Property
prop_expectedBase64 = withTests 1000 . property $ do
  c <- CTag 22 <$> forAll genCbor
  tripping c toExpectedBase64 fromExpectedBase64

prop_expectedBase16 :: Property
prop_expectedBase16 = withTests 1000 . property $ do
  c <- CTag 23 <$> forAll genCbor
  tripping c toExpectedBase16 fromExpectedBase16

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
  tripping u toEncodedURI fromEncodedURI

prop_expectedMime :: Property
prop_expectedMime = withTests 1000 . property $ do
  t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  tripping t toMIME fromMIME
