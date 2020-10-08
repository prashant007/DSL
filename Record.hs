module Record where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List
import Text.Printf
import Data.Tuple.OneTuple (only,OneTuple(..))
import Data.Maybe

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

mkRec :: Ord a => [(a,Double)] -> Rec a
mkRec = Rec . M.fromList

onRec :: Ord a => (M.Map a Double -> M.Map b Double) -> Rec a -> Rec b
onRec f =  Rec . f . unRec

mapRec :: Ord a => (Double -> Double) -> Rec a -> Rec a
mapRec f =  Rec . M.map f . unRec

mapRecWithKey :: Ord a => (a -> Double -> Double) -> Rec a -> Rec a
mapRecWithKey f = onRec (M.mapWithKey f)

mapRec2 :: Ord a => (Double -> Double -> Double) -> Rec a -> Rec a -> Rec a
mapRec2 g x y = Rec $ merge preserveMissing preserveMissing
                      (zipWithMatched (\_->g)) (unRec x) (unRec y)

zipMap :: Ord k => (k -> a -> b -> c) ->  M.Map k a -> M.Map k b -> M.Map k c
zipMap f = merge dropMissing dropMissing (zipWithMatched f)

subRec :: (a -> Bool) -> Rec a -> Rec a
subRec f = Rec . M.filterWithKey (\k _ -> f k) . unRec

foldRec :: (Double -> b -> b) -> b -> Rec a -> b
foldRec f x =  M.foldr f x . unRec

foldRecWithKey :: (a -> Double -> b -> b) -> b -> Rec a -> b
foldRecWithKey f x = M.foldrWithKey f x . unRec

groupByKeys :: (Ord k,Ord k') => (k -> k') -> M.Map k a -> M.Map k' (M.Map k a)
groupByKeys f = M.mapKeysWith M.union f . M.mapWithKey (\x y -> M.fromList [(x,y)])

-- create and group records based on a function
groupRecBy :: (Ord a,Ord b) => (a -> b) -> Rec a -> M.Map b (Rec a)
groupRecBy f = M.map Rec . groupByKeys f . unRec


-- Some Useful functions on record

-- loookup in a record
lookupRec :: Ord a => a -> Rec a -> Double
lookupRec a = fromJust . M.lookup a . unRec

-- inserting in a record
insertRec :: Ord a => a -> Double -> Rec a -> Rec a
insertRec a n = onRec (M.insert a n)

-- deleting from a record
deleteRec :: Ord a => a -> Rec a -> Rec a
deleteRec a = onRec (M.delete a)

-- summing the values in a record
sumRec :: Rec a -> Double
sumRec = foldRec (\x y -> x + y) 0

-- converting type a of Rec a to OneTuple a
mkOneTupleRec :: Ord a => Rec a -> Rec (OneTuple a)
mkOneTupleRec = onRec (M.mapKeys OneTuple)


type Percent = Double

mkPercent :: Double -> Double -> Percent
mkPercent s v = abs(v/s)*100

percentRec :: Ord a => Rec a -> Rec a
percentRec r = mapRec (mkPercent (sumRec r)) r

-- first component shows how many decimal places are to be shown
-- second argument is any string to be concatenated at end. 
showPairD :: Show a => Int -> String -> (a,Double) -> String
showPairD n s (x,y) = show x ++ " -> " ++ printf ("%."++show n++"f") y ++ s 

showSet :: [String] -> String
showSet xs = "{" ++ intercalate ", " xs ++ "}"

showSetLn :: [String] -> String
showSetLn xs = "{" ++ intercalate ",\n " xs ++ "}"

-- show record values as percentages
showRec :: Show a => Int -> String -> Rec a -> String 
showRec n s = showSet . map (showPairD n s) . fromRec

instance Show a => Show (Rec a) where
  show = showRec 4 ""

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
