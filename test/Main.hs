{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.ByteString         (ByteString, toStrict)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.Cbor
import           Data.Cbor.Decoder
import           Data.Cbor.Encoder
import           Data.Cbor.Parser
import           Data.Cbor.Tags
import           Data.Cbor.Util
import           Data.Int
import           Data.Parser
import           Hedgehog
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range
import           Test.Data
import           Test.Roundtrip
import           Test.Util

prop_negativeMappingBoundry :: Property
prop_negativeMappingBoundry = property $ do
  v :: Int64 <- fmap fromIntegral $ forAll $ Gen.choice
    [ Gen.integral $ Range.linear (-1) 1
    , Gen.integral $ Range.linear (neg2_64 - 1) (neg2_64 + 1)
    ]
  if neg2_64 <= fromIntegral v && v <= -1
  then toNegative v === pure (fromIntegral $ abs $ v + 1)
  else toNegative v === Nothing
  where
    neg2_64 = -bounds64Bit

checkCBOR :: MonadTest m => Cbor -> ByteString -> m ()
checkCBOR c s = pure c === fmap snd (runParser cbor s)

testNaNs :: MonadTest m => Cbor -> ByteString -> m ()
testNaNs c s = case c of
  CHalf n -> if isNaN n
    then case fmap snd (runParser cbor s) of
      Right (CHalf n') -> assert $ isNaN n'
      _                -> failure
    else checkCBOR c s
  CFloat n -> if isNaN n
    then case fmap snd (runParser cbor s) of
      Right (CFloat n') -> assert $ isNaN n'
      _                 -> failure
    else checkCBOR c s
  CDouble n -> if isNaN n
    then case fmap snd (runParser cbor s) of
      Right (CDouble n') -> assert $ isNaN n'
      _                  -> failure
    else checkCBOR c s
  _ -> checkCBOR c s

prop_rfcTestStrings :: Property
prop_rfcTestStrings = withTests 1000 . property $ do
  (c, s, bignum, negative) <- forAll $ Gen.choice $ pure <$> rfcTestStrings
  annotate $ "Supplied hex: " <> show (Hex s)
  annotate $ "Encoded hex : " <> either id (show . Hex . toStrict . toLazyByteString) (encode c)
  testNaNs c s
  bignum === either (const Nothing) pure (withBigNum pure c)
  case c of
    CNegative w -> pure (fromNegative w) === fmap fromIntegral negative
    _           -> pure ()

main :: IO Bool
main = checkParallel $
  Group "CBOR"
  [ ("CBOR Roundtrip", prop_roundtrip)
  , ("Negative mapping", prop_negativeMapping)
  , ("Negative mapping boundry", prop_negativeMappingBoundry)
  , ("RFC test strings", prop_rfcTestStrings)
  , ("DateTime roundtrip", prop_dateTime)
  , ("Unix epoch roundtrip", prop_unixEpoch)
  , ("BigNum roundtrip", prop_bigNum)
  , ("BigFloat roundtrip", prop_bigFloat)
  , ("BigFloat values", prop_bigFloatValues)
  , ("BigFrac roundtrip", prop_bigFrac)
  , ("BigFrac values", prop_bigFracValues)
  , ("Encoded CBOR roundtrip", prop_encodedCbor)
  , ("Expected Base64URL encoding", prop_expectedBase64Url)
  , ("Expected Base64 encoding", prop_expectedBase64)
  , ("Expected Base16 encoding", prop_expectedBase16)
  , ("Expected URI encoding", prop_encodedURI)
  , ("Expected MIME encoding", prop_expectedMime)
  , ("Expected MIME ByteString encoding", prop_expectedMimeByteString)
  ]
