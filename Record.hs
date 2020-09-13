{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module Record where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List
import Text.Printf
import Data.Tuple.OneTuple (only,OneTuple)


class (Bounded a,Enum a,Ord a) => Set a where
  members :: [a]
  members = enumFromTo minBound maxBound


data Rec a  = Rec {unRec :: M.Map a Double}


instance Show a => Show (Rec a) where
  show ts = let ts' = map (\(x,y) -> show x ++ " -> " ++ printf "%.2f" y) (fromRec ts)
            in "{" ++ intercalate ",\n " ts' ++ "}"

mkRec :: Ord a => [(a,Double)] -> Rec a
mkRec = Rec . M.fromList

fromRec :: Rec a -> [(a,Double)]
fromRec = M.toList . unRec

emptyRec :: Ord a => Rec a
emptyRec = mkRec []

-- Normalized Record
--

data Norm a = Norm {unNorm :: M.Map a Double}

instance Show a => Show (Norm a) where
  show ts = let ts' = map (\(x,y) -> show x ++ " -> " ++ printf "%.1f" (y*100)) (fromNorm ts)
            in "{" ++ intercalate ",\n " ts' ++ "}"


mkNorm :: Ord a => [(a,Double)] -> Norm a
mkNorm = Norm . M.fromList

fromNorm :: Norm a -> [(a,Double)]
fromNorm = M.toList . unNorm

recToNorm :: Ord a => Rec a -> Norm a 
recToNorm = mkNorm.fromRec 

onNorm :: Ord a => (Double -> Double -> Double) -> Norm a -> Norm a -> Norm a
onNorm g x y =  Norm $ merge preserveMissing preserveMissing
                       (zipWithMatched (\_->g)) (unNorm x) (unNorm y)

mapNorm :: Ord a => (Double -> Double) -> Norm a -> Norm a
mapNorm f =  mkNorm.map (\(x,y) -> (x,f y)).fromNorm

instance Ord a => Num (Norm a) where
  (+) = onNorm (+)
  (*) = onNorm (*)
  (-) = onNorm (-)
  negate = mapNorm negate
  abs    = mapNorm abs
  signum = mapNorm signum
  fromInteger x = undefined

diff :: Ord a => Norm a -> Norm a -> Norm a
diff = onNorm (-)

(-->) :: a -> v -> (a,v)
x --> y = (x,y)

-- Record value distribution
--
type Spread o = [(o,Double)]


emptyNorm :: Ord a => Norm a
emptyNorm = mkNorm []
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
