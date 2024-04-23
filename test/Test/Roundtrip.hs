module Test.Roundtrip where

import Data.ByteString.Lazy qualified as BL
import Data.Cbor
import Data.Cbor.Decoder
import Data.Cbor.Parser
import Data.Cbor.Serialiser
import Data.Cbor.Tags
import Data.Parser
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Time.Clock.System
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Util
import Data.Word
import Data.Cbor.Util

genCBOR :: Gen Cbor
genCBOR = Gen.sized $ \size -> do
  let collectionSize = Range.linear 0 $ unSize size
      smallerCBOR = Gen.scale (`div` 2) genCBOR
  Gen.choice
    [ CUnsigned <$> Gen.integral bounded,
      CNegative <$> Gen.integral bounded,
      CByteString <$> Gen.bytes collectionSize,
      CByteStringLazy . BL.fromStrict <$> Gen.bytes collectionSize,
      CText <$> Gen.text collectionSize Gen.unicode,
      CTextLazy . TL.fromStrict <$> Gen.text collectionSize Gen.unicode,
      CArray <$> Gen.list collectionSize smallerCBOR,
      CMap <$> Gen.map collectionSize ((,) <$> smallerCBOR <*> smallerCBOR),
      CTag <$> Gen.integral bounded <*> smallerCBOR,
      CSimple <$> Gen.filter (\x -> x < 24 || 31 < x) (Gen.integral bounded),
      CHalf <$> Gen.realFloat (Range.linearFrac (-inf) inf),
      CFloat <$> Gen.realFloat (Range.linearFrac (-inf) inf),
      CDouble <$> Gen.realFloat (Range.linearFrac (-inf) inf)
    ]
  where
    inf :: (Floating a) => a
    inf = 1 / 0
    bounded :: (Integral a, Bounded a) => Range.Range a
    bounded = Range.linear minBound maxBound

prop_roundtrip :: Property
prop_roundtrip = withTests 1000 . property $ do
  c <- forAll genCBOR
  tripping c serialise parse
  where
    serialise :: Cbor -> Either String Hex
    serialise = fmap Hex . encode
    parse :: Either String Hex -> Either Error Cbor
    parse = either (Left . Unexpected) (fmap snd . runParser cbor . unHex)

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

prop_unixEpoch :: Property
prop_unixEpoch = withTests 1000 . property $ do
  unixTime <- forAll $ Gen.integral $ Range.linear 0 (fromIntegral $ maxBound @Word32)
  let time = systemToUTCTime $ MkSystemTime unixTime 0
  tripping (pure time) fromUnixEpoch' toUnixEpoch'
  pure ()
  where
    fromUnixEpoch' :: Maybe UTCTime -> Maybe Cbor
    fromUnixEpoch' Nothing = Nothing
    fromUnixEpoch' (Just t) = fromUnixEpoch t
    toUnixEpoch' :: Maybe Cbor -> Maybe (Maybe UTCTime)
    toUnixEpoch' Nothing = Nothing
    toUnixEpoch' (Just c) = pure $ toUnixEpoch c

prop_bigNum :: Property
prop_bigNum = withTests 1000 . property $ do
  n <- forAll $ Gen.choice
    [ Gen.integral $ Range.linear bounds64Bit (bounds64Bit*2)
    , Gen.integral $ Range.linear (negate $ bounds64Bit*2) (negate bounds64Bit - 1)
    ]
  tripping (pure n) toBigNum' fromBigNum'
  where
    toBigNum' :: Maybe Integer -> Maybe HexCbor
    toBigNum' Nothing = Nothing
    toBigNum' (Just n) = HexCbor <$> toBigNum n
    fromBigNum' :: Maybe HexCbor -> Maybe (Maybe Integer)
    fromBigNum' Nothing = Nothing
    fromBigNum' (Just n) = pure $ fromBigNum $ unHexCbor n