module Data.Cbor.Zipper where

import Data.Cbor
import Numeric.Natural
import Data.Map qualified as M
import Data.Map (Map)

data Context
  = Top
  | Member Cbor (Map Cbor Cbor) Context
  | Entry [Cbor] [Cbor] Context
  deriving Show

data Location = Location Cbor Context
  deriving Show

anchor :: Cbor -> Location
anchor v = Location v Top

value :: Location -> Cbor
value (Location c _) = c

rebuild :: Location -> Cbor
rebuild (Location v Top) = v
rebuild (Location v (Member k m c)) =
  let m' = M.insert k v m
  in rebuild $ Location (CMap m') c
rebuild (Location v (Entry l r c)) =
  rebuild $ Location (CArray $ foldr (:) (v:r) l) c

child :: Cbor -> Location -> Maybe Location
child k (Location (CMap m) c) = do
  v <- M.lookup k m
  pure . Location v $ Member k (M.delete k m) c
child _ _ = Nothing

sibling :: Cbor -> Location -> Maybe Location
sibling k (Location v' (Member k' m c)) = do
  v <- M.lookup k m
  pure . Location v $ Member k (M.insert k' v' $ M.delete k m) c
sibling _ _ = Nothing

next :: Location -> Maybe Location
next (Location v (Entry l r c)) = case r of
  (x:xs) -> pure . Location x $ Entry (v:l) xs c
  _      -> Nothing
next _ = Nothing

back :: Location -> Maybe Location
back (Location v (Entry l r c)) = case l of
   (x:xs) -> pure . Location x $ Entry xs (v:r) c
   _      -> Nothing
back _ = Nothing

forward :: Natural -> Location -> Maybe Location
forward 0 l = pure l
forward i l = next l >>= forward (i - 1)

backward :: Natural -> Location -> Maybe Location
backward 0 l = pure l
backward i l = back l >>= backward (i - 1)

jump :: Integral i => i -> Location -> Maybe Location
jump i =
  if i < 0
  then backward . fromIntegral $ abs i
  else forward $ fromIntegral i

firstEntry :: Location -> Maybe Location
firstEntry (Location (CArray a) c) = case a of
  []     -> Nothing
  (x:xs) -> pure . Location x $ Entry [] xs c
firstEntry _ = Nothing

lastEntry :: Location -> Maybe Location
lastEntry (Location (CArray a) c) = case reverse a of
  []     -> Nothing
  (x:xs) -> pure . Location x $ Entry xs [] c
lastEntry _ = Nothing

up :: Location -> Maybe Location
up (Location _ Top) = Nothing
up (Location v (Member k m c)) =
  let m' = M.insert k v m
  in pure $ Location (CMap m') c
up (Location v (Entry l r c)) =
  pure $ Location (CArray $ foldr (:) (v:r) l) c

--
-- Modify the current value
--
replace :: Cbor -> Location -> Location
replace v (Location _ c) = Location v c

modify :: (Cbor -> Cbor) -> Location -> Location
modify f (Location v c) = Location (f v) c

--
-- Add values next to the current location
--
addSibling :: Cbor -> Cbor -> Location -> Maybe Location
addSibling k v (Location v' (Member k' m c)) = pure $
  Location v (Member k (M.insert k' v' m) c)
addSibling _ _ _ = Nothing

addBefore :: Cbor -> Location -> Maybe Location
addBefore v (Location v' (Entry l r c)) = pure $
  Location v (Entry l (v':r) c)
addBefore _ _ = Nothing

addAfter :: Cbor -> Location -> Maybe Location
addAfter v (Location v' (Entry l r c)) = pure $
  Location v (Entry (v':l) r c)
addAfter _ _ = Nothing

--
-- Remove values next to the current location
--
removeBefore :: Location -> Maybe Location
removeBefore (Location v (Entry l r c)) = case l of
  (_:xs) -> pure . Location v $ Entry xs r c
  _      -> Nothing
removeBefore _ = Nothing

removeAfter :: Location -> Maybe Location
removeAfter (Location v (Entry l r c)) = case r of
  (_:xs) -> pure . Location v $ Entry l xs c
  _      -> Nothing
removeAfter _ = Nothing

removeSibling :: Cbor -> Location -> Maybe Location
removeSibling k (Location v (Member k' m c)) =
  pure . Location v $ Member k' (M.delete k m) c
removeSibling _ _ = Nothing
