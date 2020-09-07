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


-- Used to represent beneficial or non-beneficial attributes
--
data Valence = Pos | Neg

class Ord a => AttrValence a where
   valence :: a -> Valence
   valence _ = Pos

data Rec a = Rec {unRec :: M.Map a Double}

mkRec :: Ord a => [(a,Double)] -> Rec a
mkRec = Rec . M.fromList

fromRec :: Rec a -> [(a,Double)]
fromRec = M.toList . unRec

onRec :: Ord a => (Double -> Double -> Double) -> Rec a -> Rec a -> Rec a
onRec g x y =  Rec $ merge preserveMissing preserveMissing
                       (zipWithMatched (\_->g)) (unRec x) (unRec y)

mapRec :: Ord a => (Double -> Double) -> Rec a -> Rec a
mapRec f =  mkRec.map (\(x,y) -> (x,f y)).fromRec

instance Ord a => Num (Rec a) where
  (+) = onRec (+)
  (*) = onRec (*)
  (-) = onRec (-)
  negate = mapRec negate
  abs    = mapRec abs
  signum = mapRec signum
  fromInteger x = undefined


-- Needed?
--
{-
add :: Ord a => Rec a -> Rec a -> Rec a
add = onRec (+)
-}

diff :: Ord a => Rec a -> Rec a -> Rec a
diff = onRec (-)

total :: Ord a => Rec a -> Double
total = M.foldr (+) 0 . unRec


instance Show a => Show (Rec a) where
  show ts = let ts' = map (\(x,y) -> show x ++ " -> " ++ printf "%.3f" y) (fromRec ts)
            in "{" ++ intercalate ",\n " ts' ++ "}"

(-->) :: a -> v -> (a,v)
x --> y = (x,y)

-- Record value distribution
--
type Spread o = [(o,Double)]

emptyRec :: Ord a => Rec a
emptyRec = mkRec []

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
