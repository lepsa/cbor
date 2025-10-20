module Data.Cbor.Util where
import           Data.Word

majorBits :: Word8
majorBits = 0b11100000

minorBits :: Word8
minorBits = 0b00011111

breakByte :: Word8
breakByte = 0b111_11111

toInt :: (Integral a, Num b) => a -> b
toInt = fromInteger . toInteger

bounds64Bit :: Integer
bounds64Bit = 2 ^ (64 :: Integer)
