{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.ByteString.Builder as BSB
import           Data.Cbor
import           Data.Cbor.Encoder       (encode)
import           Data.Cbor.Parser        (cbor)
import           Data.Parser             (Parser (runParser))
import           Hedgehog
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range

genCbor:: Gen Cbor
genCbor = Gen.sized $ \(Range.Size size) -> Gen.choice
  [ CUnsigned   <$> Gen.word64      (Range.linear 0 maxBound)
  , CNegative   <$> Gen.word64      (Range.linear 0 maxBound)
  , CByteString <$> Gen.bytes       (Range.linear 0 size)
  , CText       <$> Gen.text        (Range.linear 0 size) Gen.unicodeAll
  , CArray      <$> Gen.list        (Range.linear 0 size) (halfSize genCbor)
  , CMap        <$> Gen.map         (Range.linear 0 size) ((,) <$> halfSize genCbor <*> halfSize genCbor)
  , CTag        <$> Gen.word64      (Range.linear 0 maxBound) <*> halfSize genCbor
  , CSimple     <$> Gen.word8       (Range.linear 0 maxBound)
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
prop_roundtrip = withTests 1000 $ property $ do
  xs <- forAll $ Gen.resize 100 genCbor
  let encoded = encode xs
  case encoded of
    Left _ -> pure ()
    Right bs -> do
      let lbs = BSB.toLazyByteString bs
      annotate . show $ BSB.lazyByteStringHex lbs
      either
        (fail . show)
        (\(bs', xs') -> do
          xs === xs'
          bs' === mempty
        )
        $ runParser cbor lbs
  pure ()

main :: IO Bool
main = do
  checkParallel $$discover
