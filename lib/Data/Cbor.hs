module Data.Cbor where

import Data.ByteString
import Data.Map
import Data.Text
import Data.Word
import Numeric.Half
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL

data Cbor
  = CUnsigned Word64
  | CNegative Word64
  | CByteString ByteString
  | CByteStringLazy BL.ByteString
  | CText Text
  | CTextLazy TL.Text
  | CArray [Cbor]
  | CMap (Map Cbor Cbor)
  | CTag Word64 Cbor
  | CSimple Word8
  | CHalf Half
  | CFloat Float
  | CDouble Double
  deriving (Show, Eq, Ord)