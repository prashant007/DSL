{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,KindSignatures,DataKinds #-}

module Record where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List
import Text.Printf
import Data.Tuple.OneTuple (only,OneTuple)


-- Syntactic sugar for pairs used in maps
--
(-->) :: a -> v -> (a,v)
x --> y = (x,y)


-- Records
--
data Rec a = Rec {unRec :: M.Map a Double}

emptyRec :: Ord a => Rec a
emptyRec = mkRec []

mkRec :: Ord a => [(a,Double)] -> Rec a
mkRec = Rec . M.fromList

fromRec :: Rec a -> [(a,Double)]
fromRec = M.toList . unRec

onRec :: Ord a => (M.Map a Double -> M.Map b Double) -> Rec a -> Rec b
onRec f =  Rec . f . unRec

mapRec :: Ord a => (Double -> Double) -> Rec a -> Rec a
mapRec f =  Rec . M.map f . unRec

mapRec2 :: Ord a => (Double -> Double -> Double) -> Rec a -> Rec a -> Rec a
mapRec2 g x y =  Rec $ merge preserveMissing preserveMissing
                       (zipWithMatched (\_->g)) (unRec x) (unRec y)

subRec :: (a -> Bool) -> Rec a -> Rec a
subRec f = Rec . M.filterWithKey (\k _ -> f k) . unRec


-- Printing record values
--
-- showPair :: (Show a,Show b) => (a,b) -> String
-- showPair (x,y) = show x ++ " -> " ++ show y

showPairD :: Show a => (a,Double) -> String
showPairD (x,y) = show x ++ " -> " ++ printf "%.2f" y

showSetLn :: [String] -> String
showSetLn xs = "{" ++ intercalate ",\n " xs ++ "}"

instance Show a => Show (Rec a) where
  show = showSetLn . map showPairD . fromRec


-- Records as numbers
--
instance Ord a => Num (Rec a) where
  (+) = mapRec2 (+)
  (*) = mapRec2 (*)
  (-) = mapRec2 (-)
  negate = mapRec negate
  abs    = mapRec abs
  signum = mapRec signum
  fromInteger x = undefined

-- diff :: Ord a => Rec a -> Rec a -> Rec a
-- diff = mapRec2 (-)


-- SubDim type class projects an element from a tuple
--
class SubDim a b | a -> b where
  proj :: a -> b

instance SubDim (OneTuple a) a where proj = only

instance SubDim (a,b) a where proj = fst
instance SubDim (a,b) b where proj = snd

instance SubDim (a,b,c) a where proj (a,b,c) = a
instance SubDim (a,b,c) b where proj (a,b,c) = b
instance SubDim (a,b,c) c where proj (a,b,c) = c

instance SubDim (a,b,c,d) a where proj (a,b,c,d) = a
instance SubDim (a,b,c,d) b where proj (a,b,c,d) = b
instance SubDim (a,b,c,d) c where proj (a,b,c,d) = c
instance SubDim (a,b,c,d) d where proj (a,b,c,d) = d
