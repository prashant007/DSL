{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,KindSignatures,DataKinds #-}

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


-- Syntactic sugar for pairs used in maps
--
(-->) :: a -> v -> (a,v)
x --> y = (x,y)


-- Records
--
data Rec a = Rec {unRec :: M.Map a Double}

fromRec :: Rec a -> [(a,Double)]
fromRec = M.toList . unRec

mkRec :: Ord a => [(a,Double)] -> Rec a
mkRec = Rec . M.fromList

emptyRec :: Ord a => Rec a
emptyRec = mkRec []

onRec2 :: Ord a => (Double -> Double -> Double) -> Rec a -> Rec a -> Rec a
onRec2 g x y =  Rec $ merge preserveMissing preserveMissing
                       (zipWithMatched (\_->g)) (unRec x) (unRec y)

onRec :: Ord a => (Double -> Double) -> Rec a -> Rec a
onRec f =  mkRec . map (\(x,y) -> (x,f y)) . fromRec


-- Printing record values
--
showPair :: Show a => (a,Double) -> String
showPair (x,y) = show x ++ " -> " ++ printf "%.2f" y

showSetLn :: [String] -> String
showSetLn xs = "{" ++ intercalate ",\n " xs ++ "}"

instance Show a => Show (Rec a) where
  show ts = showSetLn (map showPair (fromRec ts))


-- Records as numbers
--
instance Ord a => Num (Rec a) where
  (+) = onRec2 (+)
  (*) = onRec2 (*)
  (-) = onRec2 (-)
  negate = onRec negate
  abs    = onRec abs
  signum = onRec signum
  fromInteger x = undefined

diff :: Ord a => Rec a -> Rec a -> Rec a
diff = onRec2 (-)


-- Record value distribution
--
type NumDist o = [(o,Double)]


-- Projector type class projects an element from a tuple
--
class Projector a b | a -> b where
  proj :: a -> b


instance Projector (OneTuple a) a where proj = only

instance Projector (a,b) a where proj = fst
instance Projector (a,b) b where proj = snd

instance Projector (a,b,c) a where proj (a,b,c) = a
instance Projector (a,b,c) b where proj (a,b,c) = b
instance Projector (a,b,c) c where proj (a,b,c) = c

instance Projector (a,b,c,d) a where proj (a,b,c,d) = a
instance Projector (a,b,c,d) b where proj (a,b,c,d) = b
instance Projector (a,b,c,d) c where proj (a,b,c,d) = c
instance Projector (a,b,c,d) d where proj (a,b,c,d) = d
