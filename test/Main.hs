{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Data.Cbor
import Data.Cbor.Parser
import Data.Cbor.Serialiser
import Data.Parser
import Data.ByteString (ByteString, unpack, pack)
import Numeric
import qualified Numeric.IEEE as IEEE
import Numeric.Half
import Data.Cbor.Decoder
import Data.Cbor.Tags
import Data.Cbor.Util
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.List as L

genCBOR :: Gen Cbor
genCBOR = Gen.sized $ \size -> do
  let collectionSize = Range.linear 0 $ unSize size
      smallerCBOR = Gen.scale (`div` 2) genCBOR
  Gen.choice
    [ CUnsigned <$> Gen.integral bounded
    , CNegative <$> Gen.integral bounded
    , CByteString <$> Gen.bytes collectionSize
    , CByteStringLazy . BL.fromStrict <$> Gen.bytes collectionSize
    , CText <$> Gen.text collectionSize Gen.unicode
    , CTextLazy . TL.fromStrict <$> Gen.text collectionSize Gen.unicode
    , CArray <$> Gen.list collectionSize smallerCBOR
    , CMap <$> Gen.map collectionSize ((,) <$> smallerCBOR <*> smallerCBOR)
    , CTag <$> Gen.integral bounded <*> smallerCBOR
    , CSimple <$> Gen.filter (\x -> x < 24 || 31 < x) (Gen.integral bounded)
    , CHalf <$> Gen.realFloat (Range.linearFrac (-inf) inf)
    , CFloat <$> Gen.realFloat (Range.linearFrac (-inf) inf)
    , CDouble <$> Gen.realFloat (Range.linearFrac (-inf) inf)
    ]
  where
    inf :: Floating a => a
    inf = 1/0
    bounded :: (Integral a, Bounded a) => Range.Range a
    bounded = Range.linear minBound maxBound

-- Print out the serialised CBOR as hex so that it can be pasted into external
-- tools to help with debugging
newtype Hex = Hex { unHex :: ByteString }
  deriving (Eq, Ord)
instance Show Hex where
  show = concatMap f . unpack . unHex
    where
      f w =
        let s = w `showHex` ""
        in if length s < 2 then '0' : s else s

fromHex :: String -> ByteString
fromHex s = pack $ fmap fst . L.uncons . readHex <$> pairs s
  where
    pairs [] = []
    pairs (a:b:s') = [a,b] : pairs s'
    pairs _ = error "Could not read hex string"

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

prop_negativeMappingBoundry :: Property
prop_negativeMappingBoundry = property $ do
  v <- forAll $ Gen.choice
    [ Gen.integral $ Range.linear (-1) 1
    , Gen.integral $ Range.linear (neg2_64 - 1) (neg2_64 + 1)
    ]
  if neg2_64 <= v && v <= -1
  then toNegative v === pure (fromInteger $ abs $ v + 1)
  else toNegative v === Nothing
  where
    neg2_64 :: Integer
    neg2_64 = -bounds64Bit

-- Test values from RFC8949, Appendix A
-- The format of the list is the CBOR value, the hex string, the expected bignum value, the expected negative value
rfcTestStrings :: [(Cbor, ByteString, Maybe Integer, Maybe Integer)]
rfcTestStrings = (\(a, b, c, d) -> (a, fromHex b, c, d)) <$>
  [ (CUnsigned 0, "00", Nothing, Nothing)
  , (CUnsigned 1, "01", Nothing, Nothing)
  , (CUnsigned 10, "0a", Nothing, Nothing)
  , (CUnsigned 23, "17", Nothing, Nothing)
  , (CUnsigned 24, "1818", Nothing, Nothing)
  , (CUnsigned 25, "1819", Nothing, Nothing)
  , (CUnsigned 100, "1864", Nothing, Nothing)
  , (CUnsigned 1000, "1903e8", Nothing, Nothing)
  , (CUnsigned 1000000, "1a000f4240", Nothing, Nothing)
  , (CUnsigned 1000000000000, "1b000000e8d4a51000", Nothing, Nothing)
  , (CUnsigned 18446744073709551615, "1bffffffffffffffff", Nothing, Nothing)
  , (CTag 2 (CByteString (fromHex "010000000000000000")), "c249010000000000000000", pure 18446744073709551616, Nothing)
  , (CNegative 18446744073709551615, "3bffffffffffffffff", Nothing, pure (-18446744073709551616))
  , (CTag 3 (CByteString (fromHex "010000000000000000")), "c349010000000000000000", pure (-18446744073709551617), Nothing)
  , (CNegative 0, "20", Nothing, pure (-1))
  , (CNegative 9, "29", Nothing, pure (-10))
  , (CNegative 99, "3863", Nothing, pure (-100))
  , (CNegative 999, "3903e7", Nothing, pure (-1000))
  , (CHalf 0.0, "f90000", Nothing, Nothing)
  , (CHalf (-0.0), "f98000", Nothing, Nothing)
  , (CHalf 1.0, "f93c00", Nothing, Nothing)
  , (CDouble 1.1, "fb3ff199999999999a", Nothing, Nothing)
  , (CHalf 1.5, "f93e00", Nothing, Nothing)
  , (CHalf 65504.0, "f97bff", Nothing, Nothing)
  , (CFloat 100000.0, "fa47c35000", Nothing, Nothing)
  , (CFloat 3.4028234663852886e+38, "fa7f7fffff", Nothing, Nothing)
  , (CDouble 1.0e+300, "fb7e37e43c8800759c", Nothing, Nothing)
  , (CHalf 5.960464477539063e-8, "f90001", Nothing, Nothing)
  , (CHalf 0.00006103515625, "f90400", Nothing, Nothing)
  , (CHalf (-4.0), "f9c400", Nothing, Nothing)
  , (CDouble (-4.1), "fbc010666666666666", Nothing, Nothing)
  , (CHalf POS_INF, "f97c00", Nothing, Nothing)
  , (CHalf QNaN, "f97e00", Nothing, Nothing)
  , (CHalf NEG_INF, "f9fc00", Nothing, Nothing)
  , (CFloat IEEE.infinity, "fa7f800000", Nothing, Nothing)
  , (CFloat IEEE.nan, "fa7fc00000", Nothing, Nothing)
  , (CFloat (-IEEE.infinity), "faff800000", Nothing, Nothing)
  , (CDouble IEEE.infinity, "fb7ff0000000000000", Nothing, Nothing)
  , (CDouble IEEE.nan, "fb7ff8000000000000", Nothing, Nothing)
  , (CDouble (-IEEE.infinity), "fbfff0000000000000", Nothing, Nothing)
  , (cFalse, "f4", Nothing, Nothing)
  , (cTrue, "f5", Nothing, Nothing)
  , (cNull, "f6", Nothing, Nothing)
  , (cUndefined, "f7", Nothing, Nothing)
  , (CSimple 16, "f0", Nothing, Nothing)
  , (CSimple 255, "f8ff", Nothing, Nothing)
  , (CTag 0 (CText "2013-03-21T20:04:00Z"), "c074323031332d30332d32315432303a30343a30305a", Nothing, Nothing)
  , (CTag 1 (CUnsigned 1363896240), "c11a514b67b0", Nothing, Nothing)
  , (CTag 1 (CDouble 1363896240.5), "c1fb41d452d9ec200000", Nothing, Nothing)
  , (CTag 23 (CByteString $ fromHex "01020304"), "d74401020304", Nothing, Nothing)
  , (CTag 24 (either (error "Failed encoding") CByteString $ encode $ CText "IETF"), "d818456449455446", Nothing, Nothing)
  , (CTag 32 (CText "http://www.example.com"), "d82076687474703a2f2f7777772e6578616d706c652e636f6d", Nothing, Nothing)
  , (CByteString $ fromHex "", "40", Nothing, Nothing)
  , (CByteString $ fromHex "01020304", "4401020304", Nothing, Nothing)
  , (CText "", "60", Nothing, Nothing)
  , (CText "a", "6161", Nothing, Nothing)
  , (CText "IETF", "6449455446", Nothing, Nothing)
  , (CText "\"\\", "62225c", Nothing, Nothing)
  , (CText "\x00fc", "62c3bc", Nothing, Nothing)
  , (CText "\x6c34", "63e6b0b4", Nothing, Nothing)
  , (CText "\x10151", "64f0908591", Nothing, Nothing)
  , (CArray [], "80", Nothing, Nothing)
  , (CArray [CUnsigned 1, CUnsigned 2, CUnsigned 3], "83010203", Nothing, Nothing)
  , (CArray [CUnsigned 1, CArray [CUnsigned 2, CUnsigned 3], CArray [CUnsigned 4, CUnsigned 5]], "8301820203820405", Nothing, Nothing)
  , (CArray [CUnsigned 1, CUnsigned 2, CUnsigned 3, CUnsigned 4, CUnsigned 5, CUnsigned 6, CUnsigned 7, CUnsigned 8, CUnsigned 9, CUnsigned 10, CUnsigned 11, CUnsigned 12, CUnsigned 13, CUnsigned 14, CUnsigned 15, CUnsigned 16, CUnsigned 17, CUnsigned 18, CUnsigned 19, CUnsigned 20, CUnsigned 21, CUnsigned 22, CUnsigned 23, CUnsigned 24, CUnsigned 25], "98190102030405060708090a0b0c0d0e0f101112131415161718181819", Nothing, Nothing)
  , (CMap mempty, "a0", Nothing, Nothing)
  , (CMap [(CUnsigned 1, CUnsigned 2), (CUnsigned 3, CUnsigned 4)], "a201020304", Nothing, Nothing)
  , (CMap [(CText "a", CUnsigned 1), (CText "b", CArray [CUnsigned 2, CUnsigned 3])], "a26161016162820203", Nothing, Nothing)
  , (CArray [CText "a", CMap [(CText "b", CText "c")]] , "826161a161626163", Nothing, Nothing)
  , (CMap [(CText "a", CText "A"), (CText "b", CText "B"), (CText "c", CText "C"), (CText "d", CText "D"), (CText "e", CText "E")], "a56161614161626142616361436164614461656145", Nothing, Nothing)
  , (CByteStringLazy $ BL.fromStrict $ fromHex "0102030405", "5f42010243030405ff", Nothing, Nothing)
  , (CTextLazy "streaming", "7f657374726561646d696e67ff", Nothing, Nothing)
  , (CArray [], "9fff", Nothing, Nothing)
  , (CArray [CUnsigned 1, CArray [CUnsigned 2, CUnsigned 3], CArray [CUnsigned 4, CUnsigned 5]], "9f018202039f0405ffff", Nothing, Nothing)
  , (CArray [CUnsigned 1, CArray [CUnsigned 2, CUnsigned 3], CArray [CUnsigned 4, CUnsigned 5]], "9f01820203820405ff", Nothing, Nothing)
  , (CArray [CUnsigned 1, CArray [CUnsigned 2, CUnsigned 3], CArray [CUnsigned 4, CUnsigned 5]], "83018202039f0405ff", Nothing, Nothing)
  , (CArray [CUnsigned 1, CArray [CUnsigned 2, CUnsigned 3], CArray [CUnsigned 4, CUnsigned 5]], "83019f0203ff820405", Nothing, Nothing)
  , (CArray [CUnsigned 1, CUnsigned 2, CUnsigned 3, CUnsigned 4, CUnsigned 5, CUnsigned 6, CUnsigned 7, CUnsigned 8, CUnsigned 9, CUnsigned 10, CUnsigned 11, CUnsigned 12, CUnsigned 13, CUnsigned 14, CUnsigned 15, CUnsigned 16, CUnsigned 17, CUnsigned 18, CUnsigned 19, CUnsigned 20, CUnsigned 21, CUnsigned 22, CUnsigned 23, CUnsigned 24, CUnsigned 25], "9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff", Nothing, Nothing)
  , (CMap [(CText "a", CUnsigned 1), (CText "b", CArray [CUnsigned 2, CUnsigned 3])], "bf61610161629f0203ffff", Nothing, Nothing)
  , (CArray [CText "a", CMap [(CText "b", CText "c")]], "826161bf61626163ff", Nothing, Nothing)
  , (CMap [(CText "Fun", cTrue), (CText "Amt", CNegative 1)], "bf6346756ef563416d7421ff", Nothing, Nothing)
  ]

checkCBOR :: MonadTest m => Cbor -> ByteString -> m ()
checkCBOR c s = pure c === fmap snd (runParser cbor s)

testNaNs :: MonadTest m => Cbor -> ByteString -> m ()
testNaNs c s = case c of
  CHalf n -> if isNaN n
    then case fmap snd (runParser cbor s) of
      Right (CHalf n') -> assert $ isNaN n'
      _ -> failure
    else checkCBOR c s
  CFloat n -> if isNaN n
    then case fmap snd (runParser cbor s) of
      Right (CFloat n') -> assert $ isNaN n'
      _ -> failure
    else checkCBOR c s
  CDouble n -> if isNaN n
    then case fmap snd (runParser cbor s) of
      Right (CDouble n') -> assert $ isNaN n'
      _ -> failure
    else checkCBOR c s
  _ -> checkCBOR c s

prop_rfcTestStrings :: Property
prop_rfcTestStrings = withTests 1000 . property $ do
  (c, s, bignum, negative) <- forAll $ Gen.choice $ pure <$> rfcTestStrings
  annotate $ "Supplied hex: " <> show (Hex s)
  annotate $ "Encoded hex : " <> either id (show . Hex) (encode c)
  testNaNs c s
  fromBigNum c === bignum
  case c of
    CNegative w -> pure (fromNegative w) === negative
    _ -> pure ()

main :: IO Bool
main = checkParallel $
  Group "Roundtrip"
  [ ("CBOR", prop_roundtrip)
  , ("Negative mapping", prop_negativeMapping)
  , ("Negative mapping boundry", prop_negativeMappingBoundry)
  , ("RFC test strings", prop_rfcTestStrings)
  ]