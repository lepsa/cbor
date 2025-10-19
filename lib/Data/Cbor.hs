module Data.Cbor where

import           Data.ByteString
import           Data.Map
import           Data.Text
import           Data.Word
import           GHC.Float       (double2Float, float2Double)
import           Numeric.Half

data Cbor
  = CUnsigned Word64
  | CNegative Word64
  | CByteString ByteString
  | CText Text
  | CArray [Cbor]
  | CMap (Map Cbor Cbor)
  | CTag Word64 Cbor
  | CSimple Word8
  | CHalf Half
  | CFloat Float
  | CDouble Double
  deriving (Ord, Show)

cUnsigned :: Word64 -> Cbor
cUnsigned = CUnsigned

cNegative :: Word64 -> Cbor
cNegative = CNegative

cBytestring :: ByteString -> Cbor
cBytestring = CByteString

cText :: Text -> Cbor
cText = CText

cArray :: [Cbor] -> Cbor
cArray = CArray

cMap ::  Map Cbor Cbor -> Cbor
cMap = CMap

cTag :: Word64 -> Cbor -> Cbor
cTag = CTag

cTrue :: Cbor
cTrue = cSimple 21

cFalse :: Cbor
cFalse = cSimple 20

cBool :: Bool -> Cbor
cBool b = if b then cTrue else cFalse

cNull :: Cbor
cNull = cSimple 22

cUndefined :: Cbor
cUndefined = cSimple 23

cSimple :: Word8 -> Cbor
cSimple = CSimple

cHalf :: Half -> Cbor
cHalf = CHalf

cFloat :: Float -> Cbor
cFloat = CFloat

cDouble :: Double -> Cbor
cDouble = CDouble


instance Eq Cbor where
  (CUnsigned a) == (CUnsigned b)     = a == b
  (CNegative a) == (CNegative b)     = a == b
  (CByteString a) == (CByteString b) = a == b
  (CText a) == (CText b)             = a == b
  (CArray a) == (CArray b)           = a == b
  (CMap a) == (CMap b)               = a == b
  (CTag a1 a2) == (CTag b1 b2)       = a1 == b1 && a2 == b2
  (CSimple a) == (CSimple b)         = a == b
  (CHalf a) == (CHalf b)             = a == b
  (CHalf a) == (CFloat b)            = a == toHalf b
  (CHalf a) == (CDouble b)           = a == toHalf (double2Float b)
  (CFloat a) == (CFloat b)           = a == b
  (CFloat a) == (CHalf b)            = a == fromHalf b
  (CFloat a) == (CDouble b)          = a == double2Float b
  (CDouble a) == (CDouble b)         = a == b
  (CDouble a) == (CHalf b)           = a == float2Double (fromHalf b)
  (CDouble a) == (CFloat b)          = a == float2Double b
  _ == _                             = False
