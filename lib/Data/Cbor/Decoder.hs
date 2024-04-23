{-# LANGUAGE OverloadedStrings #-}

module Data.Cbor.Decoder where

import Data.Cbor
import Data.Word
import GHC.Float
import Numeric.Half
import Data.Map qualified as M
import Data.ByteString
import Data.Map
import Data.Text
import Data.Cbor.Util

-- Semantic names for simple values
cFalse, cTrue, cNull, cUndefined :: Cbor
cFalse = CSimple 20
cTrue = CSimple 21
cNull = CSimple 22
cUndefined = CSimple 23

data DecodeError
  = UnexpectedValue String Cbor
  | NotFound Cbor
  | Empty
  deriving (Eq, Ord, Show)

type Decoded a = Either DecodeError a

type Decoder i a = (i -> Decoded a) -> Cbor -> Decoded a

decodedToMaybe :: Decoded a -> Maybe a
decodedToMaybe = either (const Nothing) pure

withUnsigned :: Decoder Word64 a
withUnsigned f (CUnsigned a) = f a
withUnsigned _ a = Left $ UnexpectedValue "Expected Unsigned" a

withNegative :: Decoder Word64 a
withNegative f (CNegative a) = f a
withNegative _ a = Left $ UnexpectedValue "Expected Negative" a

withInteger :: Decoder Integer a
withInteger f (CUnsigned a) = f $ fromIntegral a
withInteger f (CNegative a) = f $ fromNegative a
withInteger _ a = Left $ UnexpectedValue "Expected Unsigned or Negative" a

withByteString :: Decoder ByteString a
withByteString f (CByteString a) = f a
withByteString _ a = Left $ UnexpectedValue "Expected ByteString" a

withText :: Decoder Text a
withText f (CText a) = f a
withText _ a = Left $ UnexpectedValue "Expected Text" a

withArray :: Decoder [Cbor] a
withArray f (CArray a) = f a
withArray _ a = Left $ UnexpectedValue "Expected Array" a

withMap :: Decoder (Map Cbor Cbor) af
withMap f (CMap a) = f a
withMap _ a = Left $ UnexpectedValue "Expected Map" a

withTag :: Decoder (Word64, Cbor) a
withTag f (CTag t a) = f (t, a)
withTag _ a = Left $ UnexpectedValue "Expected Tag" a

withFalse :: Decoder () a
withFalse f v | v == cFalse = f ()
              | otherwise = Left $ UnexpectedValue "Expected False" v

withTrue :: Decoder () a
withTrue f v | v == cTrue = f ()
             | otherwise = Left $ UnexpectedValue "Expected True" v

withBool :: Decoder Bool a
withBool f v | v == cTrue = f True
             | v == cFalse = f False
             | otherwise = Left $ UnexpectedValue "Expected Bool" v

withNull :: Decoder () a
withNull f v | v == cNull = f ()
             | otherwise = Left $ UnexpectedValue "Expected Null" v

withUndefined :: Decoder () a
withUndefined f v | v == cUndefined = f ()
withUndefined _ a = Left $ UnexpectedValue "Expected Undefined" a

withHalf :: Decoder Half a
withHalf f (CHalf a) = f a
withHalf _ a = Left $ UnexpectedValue "Expected Half" a

withFloat :: Decoder Float a
withFloat f (CFloat a) = f a
withFloat _ a = Left $ UnexpectedValue "Expected Float" a

withDouble :: Decoder Double a
withDouble f (CDouble a) = f a
withDouble _ a = Left $ UnexpectedValue "Expected Double" a

withFloating :: Decoder Double a
withFloating f (CDouble a) = f a
withFloating f (CFloat a) = f $ float2Double a
withFloating f (CHalf a) = f . float2Double $ fromHalf a
withFloating _ a = Left $ UnexpectedValue "Expected Floating" a

(.:) :: Map Cbor Cbor -> Cbor -> Decoded Cbor
m .: k = maybe (Left $ NotFound k) pure $ M.lookup k m

(.:?) :: Map Cbor Cbor -> Cbor -> Decoded (Maybe Cbor)
m .:? k = pure $ M.lookup k m

-- Mapping to and from the Word64 that represents the CBOR encoding of the negative value.
-- With BigNums we can always go from the Word64 to an Integer, but going from Integer to Word64 may
-- fail if the value isn't negative, or is outside of the expected bounds.
fromNegative :: Word64 -> Integer
fromNegative w = negate (fromIntegral w) - 1

toNegative :: Integer -> Maybe Word64
toNegative i | -1 >= i && i >= -bounds64Bit = pure $ fromInteger $ abs $ i + 1
             | otherwise = Nothing