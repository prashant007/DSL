{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,KindSignatures,DataKinds #-}

module Record where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List
import Text.Printf
import Data.Tuple.OneTuple (only,OneTuple(..))

-- Syntactic sugar for pairs used in maps
--
(-->) :: a -> v -> (a,v)
x --> y = (x,y)


-- Records
--
data Rec a = Rec {unRec :: M.Map a Double}

emptyRec :: Ord a => Rec a
emptyRec = mkRec []

fromRec :: Rec a -> [(a,Double)]
fromRec = M.toList . unRec

filterRec :: (a -> Bool) -> Rec a -> Rec a
filterRec f = Rec . M.filterWithKey (\k _ -> f k) . unRec

mkRec :: Ord a => [(a,Double)] -> Rec a
mkRec = Rec . M.fromList

onRec :: Ord a => (M.Map a Double -> M.Map b Double) -> Rec a -> Rec b
onRec f =  Rec . f . unRec

mapRec :: Ord a => (Double -> Double) -> Rec a -> Rec a
mapRec f =  Rec . M.map f . unRec

mapRec2 :: Ord a => (Double -> Double -> Double) -> Rec a -> Rec a -> Rec a
mapRec2 g x y =  Rec $ merge preserveMissing preserveMissing
                       (zipWithMatched (\_->g)) (unRec x) (unRec y)


-- create and group records based on a function 
groupRecBy :: (Ord a,Ord b) => (a -> b) -> Rec a -> [Rec a]
groupRecBy f = map mkRec . sortNGroupBy (f.fst) . fromRec 


-- create a list of lists based on a function 
sortNGroupBy :: Ord b => (a -> b) -> [a] -> [[a]]
sortNGroupBy f = groupBy ((==) `on` f) . sortBy (compare `on` f)


subRec :: (a -> Bool) -> Rec a -> Rec a
subRec f = Rec . M.filterWithKey (\k _ -> f k) . unRec

foldRec :: ([(a,Double)] -> b) -> Rec a -> b 
foldRec f = f . fromRec 

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

-- class Split a f r | a -> f r where
--   focus :: a -> f 
--   remainder :: a -> r 

-- instance Split (OneTuple a) a () where 
--   focus  = only
--   remainder _ = ()

-- instance Split (a,b) b (OneTuple a) where
--   focus  = snd 
--   remainder = OneTuple . fst 

-- instance Split (a,b) a (OneTuple b) where
--   focus  = fst 
--   remainder = OneTuple . snd 

-- instance Split (a,b,c) a (b,c) where
--   focus  (a,b,c) = a
--   remainder (a,b,c) = (b,c)

-- instance Split (a,b,c) b (a,c) where
--   focus  (a,b,c) = b
--   remainder (a,b,c) = (a,c)

-- instance Split (a,b,c) c (a,b) where
--   focus  (a,b,c) = c
--   remainder (a,b,c) = (a,b)

-- instance Split (a,b,c,d) a (b,c,d) where
--   focus  (a,b,c,d) = a
--   remainder (a,b,c,d) = (b,c,d)

-- instance Split (a,b,c,d) b (a,c,d) where
--   focus  (a,b,c,d) = b 
--   remainder (a,b,c,d) = (a,c,d)

-- instance Split (a,b,c,d) c (a,b,d) where
--   focus  (a,b,c,d) = c
--   remainder (a,b,c,d) = (a,b,d)

-- instance Split (a,b,c,d) d (a,b,c) where
--   focus  (a,b,c,d) = d
--   remainder (a,b,c,d) = (a,b,c)


class SubDim  a b | a -> b where
  focus :: a -> b 

instance SubDim  (OneTuple a) a  where 
  focus = only

instance SubDim  (a,b) a where
  focus (a,b) = a

instance SubDim  (a,b) b where
  focus (a,b) = b 

instance SubDim  (a,b,c) a where
  focus (a,b,c) = a

instance SubDim  (a,b,c) b where
  focus (a,b,c) = b 

instance SubDim  (a,b,c) c where
  focus (a,b,c) = c

instance SubDim  (a,b,c,d) a where
  focus (a,b,c,d) = a 

instance SubDim  (a,b,c,d) b where
  focus (a,b,c,d) = b 

instance SubDim  (a,b,c,d) c where
  focus (a,b,c,d) = c 

instance SubDim  (a,b,c,d) d where
  focus (a,b,c,d) = d 


class Reduce a b | a -> b where
  remainder :: a -> b 

instance Reduce (OneTuple a) ()  where 
  remainder _ = ()

instance Reduce (a,b) a  where 
  remainder (a,b) = a 

instance Reduce (a,b) b  where 
  remainder (a,b) = b 

instance Reduce (a,b,c) (a,b) where 
  remainder (a,b,c) = (a,b)

instance Reduce (a,b,c) (b,c) where 
  remainder (a,b,c) = (b,c)

instance Reduce (a,b,c) (a,c) where 
  remainder (a,b,c) = (a,c)

instance Reduce (a,b,c,d) (a,b,c) where 
  remainder (a,b,c,d) = (a,b,c)

instance Reduce (a,b,c,d) (b,c,d) where 
  remainder (a,b,c,d) = (b,c,d)

instance Reduce (a,b,c,d) (a,c,d) where 
  remainder (a,b,c,d) = (a,c,d)

instance Reduce (a,b,c,d) (a,b,d) where 
  remainder (a,b,c,d) = (a,b,d)  


-- constituents = map (reduce.mkRec) . sortNGroupBy focusElem . fromRec

