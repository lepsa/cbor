module Data.Cbor.Decoder where

import           Data.ByteString (ByteString)
import           Data.Cbor
import           Data.Map        (Map)
import           Data.Text       (Text)
import           Data.Word
import           GHC.Float
import           Numeric.Half

data DecodeError
  = UnexpectedValue Cbor
  | UnexpectedByte Word8
  deriving (Eq, Ord, Show)

type Decoded a = Either DecodeError a
type Decoder i a = (i -> Decoded a) -> Cbor -> Decoded a

withUnsigned :: Decoder Word64 a
withUnsigned f (CUnsigned a) = f a
withUnsigned _ a             = Left $ UnexpectedValue a

withNegative :: Decoder Word64 a
withNegative f (CNegative a) = f a
withNegative _ a             = Left $ UnexpectedValue a

withByteString :: Decoder ByteString a
withByteString f (CByteString a) = f a
withByteString _ a               = Left $ UnexpectedValue a

withText :: Decoder Text a
withText f (CText a) = f a
withText _ a         = Left $ UnexpectedValue a

withArray :: Decoder [Cbor] a
withArray f (CArray a) = f a
withArray _ a          = Left $ UnexpectedValue a

withMap :: Decoder (Map Cbor Cbor) a
withMap f (CMap a) = f a
withMap _ a        = Left $ UnexpectedValue a

withTag :: Decoder (Word64, Cbor) a
withTag f (CTag t a) = f (t, a)
withTag _ a          = Left $ UnexpectedValue a

withSimple :: Decoder Word8 a
withSimple f (CSimple x) = f x
withSimple _ a           = Left $ UnexpectedValue a

withBool :: Decoder Bool a
withBool f (CSimple 20) = f False
withBool f (CSimple 21) = f True
withBool _ (CSimple w)  = Left $ UnexpectedByte w
withBool _ a            = Left $ UnexpectedValue a

withNull :: Decoder () a
withNull f (CSimple 22) = f ()
withNull _ (CSimple w)  = Left $ UnexpectedByte w
withNull _ a            = Left $ UnexpectedValue a

withUndefined :: Decoder () a
withUndefined f (CSimple 23) = f ()
withUndefined _ (CSimple w)  = Left $ UnexpectedByte w
withUndefined _ a            = Left $ UnexpectedValue a

withHalf :: Decoder Half a
withHalf f (CHalf a) = f a
withHalf _ a         = Left $ UnexpectedValue a

withFloat :: Decoder Float a
withFloat f (CFloat a) = f a
withFloat f (CHalf a)  = f $ fromHalf a
withFloat _ a          = Left $ UnexpectedValue a

withDouble :: Decoder Double a
withDouble f (CDouble a) = f a
withDouble f (CFloat a)  = f $ float2Double a
withDouble f (CHalf a)   = f $ float2Double $ fromHalf a
withDouble _ a           = Left $ UnexpectedValue a
