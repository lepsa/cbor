module Data.Cbor where

import Data.Word
import Data.Text
import Data.ByteString
import Data.Map
import Numeric.Half

data Cbor
  = CUnsigned Word64
  | CNegative Word64
  | CByteString ByteString
  | CText Text
  | CArray [Cbor]
  | CMap (Map Cbor Cbor)
  | CTag Word64 Cbor
  | CFalse
  | CTrue
  | CNull
  | CUndefined
  | CSimple Word8
  | CHalf Half
  | CFloat Float
  | CDouble Double
  deriving (Eq, Ord, Show)
