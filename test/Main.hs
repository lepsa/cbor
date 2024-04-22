{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Data.Cbor
import Data.Cbor.Parser
import Data.Cbor.Serialiser
import Data.Parser

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
    , pure CFalse
    , pure CTrue
    , pure CNull
    , pure CUndefined
    , CSimple <$> Gen.filter (\x -> x < 24 || 31 < x) (Gen.integral $ Range.linear 20 maxBound)
    , CHalf <$> Gen.realFloat (Range.linearFrac (-inf) inf)
    , CFloat <$> Gen.realFloat (Range.linearFrac (-inf) inf)
    , CDouble <$> Gen.realFloat (Range.linearFrac (-inf) inf)
    ]
  where
    inf :: Floating a => a
    inf = 1/0
    bounded :: (Integral a, Bounded a) => Range.Range a
    bounded = Range.linear minBound maxBound



prop_roundtrip :: Property
prop_roundtrip = withTests 1000 . property $ do
  c <- forAll genCBOR
  tripping c encode (either (Left . Unexpected) (fmap snd . runParser cbor))

main :: IO Bool
main = checkParallel $ 
  Group "Roundtrip"
  [ ("serialise <-> parse", prop_roundtrip)
  ]