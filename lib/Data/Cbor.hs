module Data.Cbor where

import           Data.ByteString
import           Data.Cbor.Util
import           Data.Int
import           Data.Map
import qualified Data.Map.Lazy   as ML
import           Data.Text
import           Data.Word
import           GHC.Float       (float2Double)
import           Numeric.Half

data Cbor
  = CUnsigned Word64
  | CNegative Word64
  | CByteString ByteString
  | CByteStringStreaming [ByteString]
  | CText Text
  | CTextStreaming [Text]
  | CArray [Cbor]
  | CArrayStreaming [Cbor]
  | CMap (Map Cbor Cbor)
  | CMapStreaming (ML.Map Cbor Cbor)
  | CTag Word64 Cbor
  | CSimple Word8
  | CHalf Half
  | CFloat Float
  | CDouble Double
  deriving (Show)

cUnsigned :: Word64 -> Cbor
cUnsigned = CUnsigned

cNegative :: Word64 -> Cbor
cNegative = CNegative

toNegative :: Int64 -> Maybe Word64
toNegative i | i <= -1 && fromIntegral i >= -bounds64Bit = pure $ fromIntegral $ abs $ i + 1
             | otherwise = Nothing

cInteger :: Int64 -> Cbor
cInteger i = maybe
  (cUnsigned $ fromIntegral i)
  cNegative
  $ toNegative i

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
  (CUnsigned a)            == (CUnsigned b)            = a == b
  (CNegative a)            == (CNegative b)            = a == b
  (CByteString a)          == (CByteString b)          = a == b
  (CByteStringStreaming a) == (CByteStringStreaming b) = a == b
  (CText a)                == (CText b)                = a == b
  (CTextStreaming a)       == (CTextStreaming b)       = a == b
  (CArray a)               == (CArray b)               = a == b
  (CArrayStreaming a)      == (CArrayStreaming b)      = a == b
  (CMap a)                 == (CMap b)                 = a == b
  (CMapStreaming a)        == (CMapStreaming b)        = a == b
  (CTag a b)               == (CTag c d)               = a == c && b == d
  (CSimple a)              == (CSimple b)              = a == b
  (CHalf a)                == (CHalf b)                = a == b
  (CHalf a)                == (CFloat b)               = fromHalf a == b
  (CHalf a)                == (CDouble b)              = float2Double (fromHalf a) == b
  (CFloat a)               == (CFloat b)               = a == b
  (CFloat a)               == (CHalf b)                = a == fromHalf b
  (CFloat a)               == (CDouble b)              = float2Double a == b
  (CDouble a)              == (CDouble b)              = a == b
  (CDouble a)              == (CHalf b)                = a == float2Double (fromHalf b)
  (CDouble a)              == (CFloat b)               = a == float2Double b
  _ == _                                               = False

instance Ord Cbor where
  (CUnsigned a)            <= (CUnsigned b)            = a <= b
  (CNegative a)            <= (CNegative b)            = a <= b
  (CByteString a)          <= (CByteString b)          = a <= b
  (CByteStringStreaming a) <= (CByteStringStreaming b) = a <= b
  (CText a)                <= (CText b)                = a <= b
  (CTextStreaming a)       <= (CTextStreaming b)       = a <= b
  (CArray a)               <= (CArray b)               = a <= b
  (CArrayStreaming a)      <= (CArrayStreaming b)      = a <= b
  (CMap a)                 <= (CMap b)                 = a <= b
  (CMapStreaming a)        <= (CMapStreaming b)        = a <= b
  (CTag a b)               <= (CTag c d)               = if a == c then b <= d else a <= c
  (CSimple a)              <= (CSimple b)              = a <= b

  (CHalf a)                <= (CHalf b)                = a <= b
  (CHalf a)                <= (CFloat b)               = fromHalf a <= b
  (CHalf a)                <= (CDouble b)              = float2Double (fromHalf a) <= b

  (CFloat a)               <= (CFloat b)               = a <= b
  (CFloat a)               <= (CHalf b)                = a <= fromHalf b
  (CFloat a)               <= (CDouble b)              = float2Double a <= b

  (CDouble a)              <= (CDouble b)              = a <= b
  (CDouble a)              <= (CHalf b)                = a <= float2Double (fromHalf b)
  (CDouble a)              <= (CFloat b)               = a <= float2Double b

  (CUnsigned _) <= _ = True

  (CNegative _) <= (CUnsigned _) = False
  (CNegative _) <= _ = True

  (CByteString _) <= (CUnsigned _) = False
  (CByteString _) <= (CNegative _) = False
  (CByteString _) <= _ = True

  (CByteStringStreaming _) <= (CUnsigned _) = False
  (CByteStringStreaming _) <= (CNegative _) = False
  (CByteStringStreaming _) <= (CByteString _) = False
  (CByteStringStreaming _) <= _ = True

  (CText _) <= (CUnsigned _) = False
  (CText _) <= (CNegative _) = False
  (CText _) <= (CByteString _) = False
  (CText _) <= (CByteStringStreaming _) = False
  (CText _) <= _ = True

  (CTextStreaming _) <= (CUnsigned _) = False
  (CTextStreaming _) <= (CNegative _) = False
  (CTextStreaming _) <= (CByteString _) = False
  (CTextStreaming _) <= (CByteStringStreaming _) = False
  (CTextStreaming _) <= (CText _) = False
  (CTextStreaming _) <= _ = True

  (CArray _) <= (CUnsigned _) = False
  (CArray _) <= (CNegative _) = False
  (CArray _) <= (CByteString _) = False
  (CArray _) <= (CByteStringStreaming _) = False
  (CArray _) <= (CText _) = False
  (CArray _) <= (CTextStreaming _) = False
  (CArray _) <= _ = True

  (CArrayStreaming _) <= (CUnsigned _) = False
  (CArrayStreaming _) <= (CNegative _) = False
  (CArrayStreaming _) <= (CByteString _) = False
  (CArrayStreaming _) <= (CByteStringStreaming _) = False
  (CArrayStreaming _) <= (CText _) = False
  (CArrayStreaming _) <= (CTextStreaming _) = False
  (CArrayStreaming _) <= (CArray _) = False
  (CArrayStreaming _) <= _ = True

  (CMap _) <= (CUnsigned _) = False
  (CMap _) <= (CNegative _) = False
  (CMap _) <= (CByteString _) = False
  (CMap _) <= (CByteStringStreaming _) = False
  (CMap _) <= (CText _) = False
  (CMap _) <= (CTextStreaming _) = False
  (CMap _) <= (CArray _) = False
  (CMap _) <= (CArrayStreaming _) = False
  (CMap _) <= _ = True

  (CMapStreaming _) <= (CUnsigned _) = False
  (CMapStreaming _) <= (CNegative _) = False
  (CMapStreaming _) <= (CByteString _) = False
  (CMapStreaming _) <= (CByteStringStreaming _) = False
  (CMapStreaming _) <= (CText _) = False
  (CMapStreaming _) <= (CTextStreaming _) = False
  (CMapStreaming _) <= (CArray _) = False
  (CMapStreaming _) <= (CArrayStreaming _) = False
  (CMapStreaming _) <= (CMap _) = False
  (CMapStreaming _) <= _ = True

  (CTag _ _) <= (CUnsigned _) = False
  (CTag _ _) <= (CNegative _) = False
  (CTag _ _) <= (CByteString _) = False
  (CTag _ _) <= (CByteStringStreaming _) = False
  (CTag _ _) <= (CText _) = False
  (CTag _ _) <= (CTextStreaming _) = False
  (CTag _ _) <= (CArray _) = False
  (CTag _ _) <= (CArrayStreaming _) = False
  (CTag _ _) <= (CMap _) = False
  (CTag _ _) <= (CMapStreaming _) = False
  (CTag _ _) <= _ = True

  (CSimple _) <= (CUnsigned _) = False
  (CSimple _) <= (CNegative _) = False
  (CSimple _) <= (CByteString _) = False
  (CSimple _) <= (CByteStringStreaming _) = False
  (CSimple _) <= (CText _) = False
  (CSimple _) <= (CTextStreaming _) = False
  (CSimple _) <= (CArray _) = False
  (CSimple _) <= (CArrayStreaming _) = False
  (CSimple _) <= (CMap _) = False
  (CSimple _) <= (CMapStreaming _) = False
  (CSimple _) <= (CTag _ _) = False
  (CSimple _) <= _ = True

  (CHalf _) <= (CUnsigned _) = False
  (CHalf _) <= (CNegative _) = False
  (CHalf _) <= (CByteString _) = False
  (CHalf _) <= (CByteStringStreaming _) = False
  (CHalf _) <= (CText _) = False
  (CHalf _) <= (CTextStreaming _) = False
  (CHalf _) <= (CArray _) = False
  (CHalf _) <= (CArrayStreaming _) = False
  (CHalf _) <= (CMap _) = False
  (CHalf _) <= (CMapStreaming _) = False
  (CHalf _) <= (CTag _ _) = False
  (CHalf _) <= (CSimple _) = False

  (CFloat _) <= (CUnsigned _) = False
  (CFloat _) <= (CNegative _) = False
  (CFloat _) <= (CByteString _) = False
  (CFloat _) <= (CByteStringStreaming _) = False
  (CFloat _) <= (CText _) = False
  (CFloat _) <= (CTextStreaming _) = False
  (CFloat _) <= (CArray _) = False
  (CFloat _) <= (CArrayStreaming _) = False
  (CFloat _) <= (CMap _) = False
  (CFloat _) <= (CMapStreaming _) = False
  (CFloat _) <= (CTag _ _) = False
  (CFloat _) <= (CSimple _) = False

  (CDouble _) <= (CUnsigned _) = False
  (CDouble _) <= (CNegative _) = False
  (CDouble _) <= (CByteString _) = False
  (CDouble _) <= (CByteStringStreaming _) = False
  (CDouble _) <= (CText _) = False
  (CDouble _) <= (CTextStreaming _) = False
  (CDouble _) <= (CArray _) = False
  (CDouble _) <= (CArrayStreaming _) = False
  (CDouble _) <= (CMap _) = False
  (CDouble _) <= (CMapStreaming _) = False
  (CDouble _) <= (CTag _ _) = False
  (CDouble _) <= (CSimple _) = False
