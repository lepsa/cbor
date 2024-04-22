{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Cbor.Decoder where

import Data.ByteString (ByteString)
import Data.Cbor
import Data.Map (Map)
import Data.Text (Text)
import Data.Word
import GHC.Float
import Numeric.Half
import Data.Map qualified as M

data DecodeError
  = UnexpectedValue String Cbor
  | NotFound Cbor
  deriving (Eq, Ord, Show)

type Decoded a = Either DecodeError a

type Decoder i a = (i -> Decoded a) -> Cbor -> Decoded a

withUnsigned :: Decoder Word64 a
withUnsigned f (CUnsigned a) = f a
withUnsigned _ a = Left $ UnexpectedValue "Expected Unsigned" a

withNegative :: Decoder Word64 a
withNegative f (CNegative a) = f a
withNegative _ a = Left $ UnexpectedValue "Expected Negative" a

withByteString :: Decoder ByteString a
withByteString f (CByteString a) = f a
withByteString _ a = Left $ UnexpectedValue "Expected ByteString" a

withText :: Decoder Text a
withText f (CText a) = f a
withText _ a = Left $ UnexpectedValue "Expected Text" a

withArray :: Decoder [Cbor] a
withArray f (CArray a) = f a
withArray _ a = Left $ UnexpectedValue "Expected Array" a

withMap :: Decoder (Map Cbor Cbor) a
withMap f (CMap a) = f a
withMap _ a = Left $ UnexpectedValue "Expected Map" a

withTag :: Decoder (Word64, Cbor) a
withTag f (CTag t a) = f (t, a)
withTag _ a = Left $ UnexpectedValue "Expected Tag" a

withFalse :: Decoder () a
withFalse f CFalse = f ()
withFalse _ a = Left $ UnexpectedValue "Expected False" a

withTrue :: Decoder () a
withTrue f CTrue = f ()
withTrue _ a = Left $ UnexpectedValue "Expected True" a

withBool :: Decoder Bool a
withBool f CTrue = f True
withBool f CFalse = f False
withBool _ a = Left $ UnexpectedValue "Expected Bool" a

withNull :: Decoder () a
withNull f CNull = f ()
withNull _ a = Left $ UnexpectedValue "Expected Null" a

withUndefined :: Decoder () a
withUndefined f CUndefined = f ()
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