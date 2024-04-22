{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Data.Cbor
import Data.Cbor.Parser
import Data.Cbor.Serialiser
import Data.Parser
import Data.ByteString (ByteString, unpack)
import Numeric

genCBOR :: Gen Cbor
genCBOR = Gen.sized $ \size -> do
  let collectionSize = Range.linear 0 $ unSize size
      smallerCBOR = Gen.scale (`div` 2) genCBOR
  Gen.choice
    [ CUnsigned <$> Gen.integral bounded
    , CNegative <$> Gen.integral bounded
    , CByteString <$> Gen.bytes collectionSize
    , CText <$> Gen.text collectionSize Gen.unicode
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
  show = concatMap (`showHex` "") . unpack . unHex

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
    neg2_64 = -2^64

main :: IO Bool
main = checkParallel $
  Group "Roundtrip"
  [ ("CBOR", prop_roundtrip)
  , ("Negative mapping", prop_negativeMapping)
  , ("Negative mapping boundry", prop_negativeMappingBoundry)
  ]