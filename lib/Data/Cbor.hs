module Data.Cbor where

import Data.ByteString
import Data.Map
import Data.Text
import Data.Word
import Numeric.Half

-- Semantic names for simple values
cFalse, cTrue, cNull, cUndefined :: Cbor
cFalse = CSimple 20
cTrue = CSimple 21
cNull = CSimple 22
cUndefined = CSimple 23

-- Mapping to and from the Word64 that represents the CBOR encoding of the negative value.Applicative
-- With BigNums we can always go from the Word64 to an Integer, but going from Integer to Word64 may
-- fail if the value isn't negative, or is outside of the expected bounds.
fromNegative :: Word64 -> Integer
fromNegative w = negate (fromIntegral w) - 1

toNegative :: Integer -> Maybe Word64
toNegative i | -1 >= i && i >= -2^64 = pure $ fromInteger $ abs $ i + 1
             | otherwise = Nothing

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
  deriving (Show, Eq, Ord)