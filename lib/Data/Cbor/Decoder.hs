{-# LANGUAGE LambdaCase #-}

module Data.Cbor.Decoder where

import Data.Cbor
import Data.Word
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Map (Map)
import Numeric.Half
import GHC.Float

data DecodeError
  = UnexpectedValue Cbor
  deriving (Eq, Ord, Show)

type Decoded a = Either DecodeError a
type Decoder i a = (i -> Decoded a) -> Cbor -> Decoded a

withUnsigned :: Decoder Word64 a
withUnsigned f (CUnsigned a) = f a
withUnsigned _ a = Left $ UnexpectedValue a

withNegative :: Decoder Word64 a
withNegative f (CNegative a) = f a
withNegative _ a = Left $ UnexpectedValue a

withByteString :: Decoder ByteString a
withByteString f (CByteString a) = f a
withByteString _ a = Left $ UnexpectedValue a

withText :: Decoder Text a
withText f (CText a) = f a
withText _ a = Left $ UnexpectedValue a

withArray :: Decoder [Cbor] a
withArray f (CArray a) = f a
withArray _ a = Left $ UnexpectedValue a

withMap :: Decoder (Map Cbor Cbor) a
withMap f (CMap a) = f a
withMap _ a = Left $ UnexpectedValue a

withTag :: Decoder (Word64, Cbor) a
withTag f (CTag t a) = f (t, a)
withTag _ a = Left $ UnexpectedValue a

withFalse :: Decoder () a
withFalse f CFalse = f ()
withFalse _ a = Left $ UnexpectedValue a

withTrue :: Decoder () a
withTrue f CTrue = f ()
withTrue _ a = Left $ UnexpectedValue a

withBool :: Decoder Bool a
withBool f CTrue = f True
withBool f CFalse = f False
withBool _ a = Left $ UnexpectedValue a

withNull :: Decoder () a
withNull f CNull = f ()
withNull _ a = Left $ UnexpectedValue a

withUndefined :: Decoder () a
withUndefined f CUndefined = f ()
withUndefined _ a = Left $ UnexpectedValue a

withHalf :: Decoder Half a
withHalf f (CHalf a) = f a
withHalf _ a = Left $ UnexpectedValue a

withFloat :: Decoder Float a
withFloat f (CFloat a) = f a
withFloat _ a = Left $ UnexpectedValue a

withDouble :: Decoder Double a
withDouble f (CDouble a) = f a
withDouble _ a = Left $ UnexpectedValue a

withFloating :: Decoder Double a
withFloating f (CDouble a) = f a
withFloating f (CFloat  a) = f $ float2Double a
withFloating f (CHalf   a) = f . float2Double $ fromHalf a
withFloating _ a = Left $ UnexpectedValue a
