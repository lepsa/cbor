module Data.Cbor where

import Data.ByteString
import Data.Map
import Data.Text
import Data.Word
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
  deriving (Show)

instance Eq Cbor where
  -- Make sure that explicit simple values are
  -- equal to their raw simple value
  CFalse == CSimple 20 = True
  CTrue == CSimple 21 = True
  CNull == CSimple 22 = True
  CUndefined == CSimple 23 = True
  -- And in the other order
  CSimple 20 == CFalse = True
  CSimple 21 == CTrue = True
  CSimple 22 == CNull = True
  CSimple 23 == CUndefined = True
  -- Equalities for held values
  CUnsigned a == CUnsigned b = a == b
  CNegative a == CNegative b = a == b
  CByteString a == CByteString b = a == b
  CText a == CText b = a == b
  CArray a == CArray b = a == b
  CMap a == CMap b = a == b
  CTag a1 a2 == CTag b1 b2 = a1 == b1 && a2 == b2
  CFalse == CFalse = True
  CTrue == CTrue = True
  CNull == CNull = True
  CUndefined == CUndefined = True
  CSimple a == CSimple b = a == b
  CHalf a == CHalf b = a == b
  CFloat a == CFloat b = a == b
  CDouble a == CDouble b = a == b
  _ == _ = False

instance Ord Cbor where
  -- manual orderings
  compare CFalse (CSimple 20) = EQ
  compare CFalse (CSimple 21) = LT
  compare CFalse (CSimple 22) = LT
  compare CFalse (CSimple 23) = LT

  compare CTrue (CSimple 20) = GT
  compare CTrue (CSimple 21) = EQ
  compare CTrue (CSimple 22) = LT
  compare CTrue (CSimple 23) = LT

  compare CNull (CSimple 20) = GT
  compare CNull (CSimple 21) = GT
  compare CNull (CSimple 22) = EQ
  compare CNull (CSimple 23) = LT

  compare CUndefined (CSimple 20) = GT
  compare CUndefined (CSimple 21) = GT
  compare CUndefined (CSimple 22) = GT
  compare CUndefined (CSimple 23) = EQ

  compare (CSimple 20) CFalse = EQ
  compare (CSimple 21) CFalse = GT
  compare (CSimple 22) CFalse = GT
  compare (CSimple 23) CFalse = GT

  compare (CSimple 20) CTrue = LT
  compare (CSimple 21) CTrue = EQ
  compare (CSimple 22) CTrue = GT
  compare (CSimple 23) CTrue = GT
  
  compare (CSimple 20) CNull = LT
  compare (CSimple 21) CNull = LT
  compare (CSimple 22) CNull = EQ
  compare (CSimple 23) CNull = GT
  
  compare (CSimple 20) CUndefined = LT
  compare (CSimple 21) CUndefined = LT
  compare (CSimple 22) CUndefined = LT
  compare (CSimple 23) CUndefined = EQ

  -- easy value orderings
  compare (CUnsigned a) (CUnsigned b) = compare a b
  compare (CNegative a) (CNegative b) = compare a b
  compare (CByteString a) (CByteString b) = compare a b
  compare (CText a) (CText b) = compare a b
  compare (CArray a) (CArray b) = compare a b
  compare (CMap a) (CMap b) = compare a b
  compare (CTag a1 a2) (CTag b1 b2) = compare a1 b1 <> compare a2 b2
  compare CFalse CFalse = EQ
  compare CTrue CTrue = EQ
  compare CNull CNull = EQ
  compare CUndefined CUndefined = EQ
  compare (CSimple a) (CSimple b) = compare a b
  compare (CHalf a) (CHalf b) = compare a b
  compare (CFloat a) (CFloat b) = compare a b
  compare (CDouble a) (CDouble b) = compare a b
  
  -- Order values, following their major type to get some sort
  -- of binary equivalence.
  compare (CUnsigned _) _ = LT

  compare (CNegative _) (CUnsigned _) = GT
  compare (CNegative _) _ = LT

  compare (CByteString _) (CUnsigned _) = GT
  compare (CByteString _) (CNegative _) = GT
  compare (CByteString _) _ = LT

  compare (CText _) (CUnsigned _) = GT
  compare (CText _) (CNegative _) = GT
  compare (CText _) (CByteString _) = GT
  compare (CText _) _ = LT

  compare (CArray _) (CUnsigned _) = GT
  compare (CArray _) (CNegative _) = GT
  compare (CArray _) (CByteString _) = GT
  compare (CArray _) (CText _) = GT
  compare (CArray _) _ = LT

  compare (CMap _) (CUnsigned _) = GT
  compare (CMap _) (CNegative _) = GT
  compare (CMap _) (CByteString _) = GT
  compare (CMap _) (CText _) = GT
  compare (CMap _) (CArray _) = GT
  compare (CMap _) _ = LT

  compare (CTag _ _) (CUnsigned _) = GT
  compare (CTag _ _) (CNegative _) = GT
  compare (CTag _ _) (CByteString _) = GT
  compare (CTag _ _) (CText _) = GT
  compare (CTag _ _) (CArray _) = GT
  compare (CTag _ _) (CMap _) = GT
  compare (CTag _ _) _ = LT

  compare CFalse (CUnsigned _) = GT
  compare CFalse (CNegative _) = GT
  compare CFalse (CByteString _) = GT
  compare CFalse (CText _) = GT
  compare CFalse (CArray _) = GT
  compare CFalse (CMap _) = GT
  compare CFalse (CTag _ _) = GT
  compare CFalse _ = LT

  compare CTrue (CUnsigned _) = GT
  compare CTrue (CNegative _) = GT
  compare CTrue (CByteString _) = GT
  compare CTrue (CText _) = GT
  compare CTrue (CArray _) = GT
  compare CTrue (CMap _) = GT
  compare CTrue (CTag _ _) = GT
  compare CTrue CFalse = GT
  compare CTrue _ = LT

  compare CNull (CUnsigned _) = GT
  compare CNull (CNegative _) = GT
  compare CNull (CByteString _) = GT
  compare CNull (CText _) = GT
  compare CNull (CArray _) = GT
  compare CNull (CMap _) = GT
  compare CNull (CTag _ _) = GT
  compare CNull CFalse = GT
  compare CNull CTrue = GT
  compare CNull _ = LT

  compare CUndefined (CUnsigned _) = GT
  compare CUndefined (CNegative _) = GT
  compare CUndefined (CByteString _) = GT
  compare CUndefined (CText _) = GT
  compare CUndefined (CArray _) = GT
  compare CUndefined (CMap _) = GT
  compare CUndefined (CTag _ _) = GT
  compare CUndefined CFalse = GT
  compare CUndefined CTrue = GT
  compare CUndefined CNull = GT
  compare CUndefined _ = LT

  compare (CSimple _) (CUnsigned _) = GT
  compare (CSimple _) (CNegative _) = GT
  compare (CSimple _) (CByteString _) = GT
  compare (CSimple _) (CText _) = GT
  compare (CSimple _) (CArray _) = GT
  compare (CSimple _) (CMap _) = GT
  compare (CSimple _) (CTag _ _) = GT
  compare (CSimple _) CFalse = GT
  compare (CSimple _) CTrue = GT
  compare (CSimple _) CNull = GT
  compare (CSimple _) CUndefined = GT
  compare (CSimple _) _ = LT
  compare (CHalf _) (CUnsigned _) = GT
  compare (CHalf _) (CNegative _) = GT
  compare (CHalf _) (CByteString _) = GT
  compare (CHalf _) (CText _) = GT
  compare (CHalf _) (CArray _) = GT
  compare (CHalf _) (CMap _) = GT
  compare (CHalf _) (CTag _ _) = GT
  compare (CHalf _) CFalse = GT
  compare (CHalf _) CTrue = GT
  compare (CHalf _) CNull = GT
  compare (CHalf _) CUndefined = GT
  compare (CHalf _) (CSimple _) = GT
  compare (CHalf _) _ = LT

  compare (CFloat _) (CUnsigned _) = GT
  compare (CFloat _) (CNegative _) = GT
  compare (CFloat _) (CByteString _) = GT
  compare (CFloat _) (CText _) = GT
  compare (CFloat _) (CArray _) = GT
  compare (CFloat _) (CMap _) = GT
  compare (CFloat _) (CTag _ _) = GT
  compare (CFloat _) CFalse = GT
  compare (CFloat _) CTrue = GT
  compare (CFloat _) CNull = GT
  compare (CFloat _) CUndefined = GT
  compare (CFloat _) (CSimple _) = GT
  compare (CFloat _) (CHalf _) = GT
  compare (CFloat _) _ = LT
  
  compare (CDouble _) _ = GT
