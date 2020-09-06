{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module Attribute where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List
import Text.Printf
import Data.Tuple.OneTuple (only,OneTuple)

class (Bounded a,Enum a,Ord a) => Set a where
  members :: [a]
  members = enumFromTo minBound maxBound


-- Used to represent beneficial or non-beneficial attributes
--
data Valence = Pos | Neg

class Ord a => AttrValence a where
   valence :: a -> Valence
   valence _ = Pos

data Attr a = Attr {unAttr :: M.Map a Double}

mkAttr :: Ord a => [(a,Double)] -> Attr a
mkAttr = Attr . M.fromList

fromAttr :: Attr a -> [(a,Double)]
fromAttr = M.toList . unAttr

onAttr :: Ord a => (Double -> Double -> Double) -> Attr a -> Attr a -> Attr a
onAttr g x y =  Attr $ merge preserveMissing preserveMissing
                       (zipWithMatched (\_->g)) (unAttr x) (unAttr y)


mapAttr :: Ord a => (Double -> Double) -> Attr a -> Attr a 
mapAttr f =  mkAttr.map (\(x,y) -> (x,f y)).fromAttr


instance (Set a,Ord a) => Num (Attr a) where
  (+) = onAttr (+) 
  (*) = onAttr (*) 
  (-) = onAttr (-) 
  negate = mapAttr negate
  abs    = mapAttr abs
  signum = mapAttr signum
  fromInteger x = mkAttr [(head $ members,fromInteger x)]

-- Needed?
--
{-
add :: Ord a => Attr a -> Attr a -> Attr a
add = onAttr (+)
-}

diff :: Ord a => Attr a -> Attr a -> Attr a
diff = onAttr (-)

instance Show a => Show (Attr a) where
  show ts = let ts' = map (\(x,y) -> show x ++ " -> " ++ printf "%.3f" y) (fromAttr ts)
            in "{" ++ intercalate ",\n " ts' ++ "}"

(-->) :: a -> v -> (a,v)
x --> y = (x,y)

-- Attribute value distribution
--
type Spread o = [(o,Double)]

noAttributes :: Ord a => Attr a
noAttributes = mkAttr []

-- ========================== PROJECTOR =====================================
-- ==========================================================================

-- Projector type class projects an element from a tuple

class Projector a b | a -> b where
  proj :: a -> b


instance Projector (OneTuple a) a where
  proj = only

instance Projector (a,b) a where
  proj = fst

instance Projector (a,b) b where
  proj = snd

instance Projector (a,b,c) a where
  proj (a,b,c) = a

instance Projector (a,b,c) b where
  proj (a,b,c) = b

instance Projector (a,b,c) c where
  proj (a,b,c) = c

instance Projector (a,b,c,d) a where
  proj (a,b,c,d) = a

instance Projector (a,b,c,d) b where
  proj (a,b,c,d) = b

instance Projector (a,b,c,d) c where
  proj (a,b,c,d) = c

instance Projector (a,b,c,d) d where
  proj (a,b,c,d) = d
