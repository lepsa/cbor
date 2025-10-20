{-# LANGUAGE OverloadedLists #-}

module Test.Util where

import           Data.ByteString         (ByteString, singleton, unpack)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Cbor
import           Data.Cbor.Encoder
import qualified Data.List               as L
import           Data.Maybe
import           Data.Word
import           Numeric
-- Print out the serialised CBOR as hex so that it can be pasted into external
-- tools to help with debugging
newtype Hex = Hex { unHex :: ByteString }
  deriving (Eq, Ord)
instance Show Hex where
  show = concatMap f . unpack . unHex
    where
      f w =
        let s = w `showHex` ""
        in if length s < 2 then '0' : s else s

newtype HexCbor = HexCbor { unHexCbor :: Cbor }
  deriving (Eq, Ord)
instance Show HexCbor where
  show = either id (show . Hex . toStrict . toLazyByteString) . encode . unHexCbor

fromHex :: String -> ByteString
fromHex = mconcat . mapMaybe (fmap (singleton . fst . fst) . L.uncons . readHex @Word8) . pairs
  where
    pairs []       = []
    pairs (a:b:s') = [a,b] : pairs s'
    pairs _        = error "Could not read hex string"
