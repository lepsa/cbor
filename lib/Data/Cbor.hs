{-# LANGUAGE OverloadedLists #-}

module Data.Cbor where

import Data.ByteString
import Data.Map
import Data.Text
import Data.Word
import Numeric.Half
import qualified Data.ByteString as B
import Numeric.Natural

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

fromNetworkOrder :: ByteString -> Natural
fromNetworkOrder = go 0
  where
    go :: Natural -> ByteString -> Natural
    go i bs = case B.uncons bs of
      Nothing -> i
      Just (b, bs') -> go (fromIntegral b + (i * 256)) bs'

toNetworkOrder :: Natural -> ByteString
toNetworkOrder = go []
  where
    go :: ByteString -> Natural -> ByteString
    go bs 0 = bs
    go bs n =
      let w = rem n 256
      in go (B.cons (fromIntegral w) bs) (div n 256)


-- RFC8949 section 3.4.3
fromBigNum :: Cbor -> Maybe Integer
fromBigNum (CTag 2 (CByteString b)) = pure $ fromIntegral $ fromNetworkOrder b -- Positive bignums
fromBigNum (CTag 3 (CByteString b)) = pure $ negate (fromIntegral $ fromNetworkOrder b) - 1 -- Negative bignums
fromBigNum _ = Nothing

toBigNum :: Integer -> Maybe Cbor
toBigNum n | n >= 2^64 = pure $ CTag 2 $ CByteString $ toNetworkOrder $ fromIntegral n
           | n <= -2^64 = pure $ CTag 3 $ CByteString $ toNetworkOrder $ fromIntegral $ negate n
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