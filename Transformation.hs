{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,InstanceSigs #-}

module Transformation where

import Data.Function
import qualified Data.Map as M
import Text.Printf
import Data.List hiding (filter)
import Prelude hiding (filter)
import Data.Tuple.OneTuple (OneTuple(..))

import Record
import Info
import MDS hiding (compare)


-- ================== GENERALIZE ==========================================
-- ========================================================================

-- The MDS explanations in general consist of explanations from various level.
-- For example, in the Car example the decision to buy a car is comprised of
-- features of the car like fuel and safety ratings as well as the people
-- advising the buyer namely the friends, and experts. These two levels are
-- independent of each other in the mind of an explanation consumer. The
-- Generalize type class realizes this need by providing an explanation in
-- terms of one level at a time.

class (Ord a,Ord b,SumOut a b) => Generalize a b | a -> b where
    generalize :: ValDiff a -> Explain b
    generalize = explain.sumOut


instance (Ord a,Ord b) => Generalize (a,b) a
instance (Ord a,Ord b) => Generalize (a,b) b

instance (Ord a,Ord b,Ord c) => Generalize (a,b,c) a
instance (Ord a,Ord b,Ord c) => Generalize (a,b,c) b
instance (Ord a,Ord b,Ord c) => Generalize (a,b,c) c

instance (Ord a,Ord b,Ord c,Ord d) => Generalize (a,b,c,d) a
instance (Ord a,Ord b,Ord c,Ord d) => Generalize (a,b,c,d) b
instance (Ord a,Ord b,Ord c,Ord d) => Generalize (a,b,c,d) c
instance (Ord a,Ord b,Ord c,Ord d) => Generalize (a,b,c,d) d

-- ================== Selector ==========================================
-- ========================================================================

-- class (Eq a, Eq b,SubDim a b) => Selector o a b | a -> b where
--   filter :: (a -> Bool) -> Info o a -> Info o a
--   filter f = onInfo (M.map (filterRec f))
--
-- only :: Selector o a b => b -> Info o a -> Info o a
-- only v = filter $ (==v) . proj
--
-- except :: Selector o a b => b -> Info o a -> Info o a
-- except v = filter $ (/=v) . proj
--
-- instance (Eq a,Eq b) => Selector o (a,b) a
-- instance (Eq a,Eq b) => Selector o (a,b) b
-- instance (Eq a,Eq b,Eq c) => Selector o (a,b,c) a
-- instance (Eq a,Eq b,Eq c) => Selector o (a,b,c) b
-- instance (Eq a,Eq b,Eq c) => Selector o (a,b,c) c

filter :: (a -> Bool) -> Info o a -> Info o a
filter f = onInfo (M.map (filterRec f))

only :: (Eq b,SubDim a b) => b -> Info o a -> Info o a
only v = filter $ (==v) . proj

except :: (Eq b,SubDim a b) => b -> Info o a -> Info o a
except v = filter $ (/=v) . proj


-- ================== SUMOUT ==============================================
-- ========================================================================

-- Sometimes we need to get rid of all but one elements in the tuples of Norm values
-- Consider the Norm val {(Friend,Fuel) -> 0.048,(Friend,Price) -> 0.032(Expert,Price) -> 0.080}.
-- Suppose, we want to to get the attr val just in terms of the friends and experts, that
-- get rid of the feature component from the tuples. Once we get rid of (sum out) the second
-- component of the tuple, the new tuple would look like this:
-- {Friend -> 0.048,Friend -> 0.032,Expert -> 0.080} which can now be rewritten as
-- {Friend -> 0.080,Expert -> 0.080}. Similarly, we could sum out the first component of the tuple
-- thereby giving us this attr val: {Fuel -> 0.048,Price -> 0.112}. SumOut is a type class
-- used to achieve this effect on attr values.

class (SubDim a b,Ord b) => SumOut a b | a -> b where
  sumOut :: Rec a -> Rec b
  sumOut = mkRec.map h.groupBy g.sortBy (compare `on` f).fromRec
    where
        h xs = ((f.head) xs,(sum.map snd) xs)
        g x y = f x == f y
        f = proj.fst

instance Ord b => SumOut (a,b) b
instance Ord a => SumOut (a,b) a
instance Ord a => SumOut (a,b,c) a
instance Ord b => SumOut (a,b,c) b
instance Ord c => SumOut (a,b,c) c
instance Ord a => SumOut (a,b,c,d) a
instance Ord b => SumOut (a,b,c,d) b
instance Ord c => SumOut (a,b,c,d) c
instance Ord d => SumOut (a,b,c,d) d


-- ===================== REDUCE =============================================
-- ==========================================================================

-- Many a times the record value may have an argument that stays the same in all
-- the elements of an Norm value. The Reduce type class provides a way, using
-- the denoise function to achieve this.

class Reduce a b | a -> b where
  rmv :: a -> b

  reduce :: (Ord a,Ord b) => Rec a -> Rec b
  reduce = mkRec.map (\(x,n) -> (rmv x,n)).fromRec

instance Reduce (OneTuple a) () where
   rmv _ = ()

instance Reduce (a,b) b where
  rmv :: (a,b) -> b
  rmv = snd

instance Reduce (a,b) a where
  rmv :: (a,b) -> a
  rmv = fst

instance Reduce (a,b,c) (b,c) where
  rmv :: (a,b,c) -> (b,c)
  rmv (a,b,c) = (b,c)

instance Reduce (a,b,c) (a,c) where
  rmv :: (a,b,c) -> (a,c)
  rmv (a,b,c) = (a,c)

instance Reduce (a,b,c) (a,b) where
  rmv :: (a,b,c) -> (a,b)
  rmv (a,b,c) = (a,b)

instance Reduce (a,b,c,d) (a,b,c) where
  rmv :: (a,b,c,d) -> (a,b,c)
  rmv (a,b,c,d) = (a,b,c)

instance Reduce (a,b,c,d) (b,c,d) where
  rmv :: (a,b,c,d) -> (b,c,d)
  rmv (a,b,c,d) = (b,c,d)

instance Reduce (a,b,c,d) (a,b,d) where
  rmv :: (a,b,c,d) -> (a,b,d)
  rmv (a,b,c,d) = (a,b,d)

instance Reduce (a,b,c,d) (a,c,d) where
  rmv :: (a,b,c,d) -> (a,c,d)
  rmv (a,b,c,d) = (a,c,d)


-- ========================== FACTORING =====================================
-- ==========================================================================

-- Consider the following record value: {(Friend,Safety) -> -0.048,(Expert,Safety) -> -0.064}
-- We observe that Safety could be factored in this Norm value giving us this pretty printed
-- representation: Safety: -0.112 (Friend:-0.048, Expert:-0.064). This factored representation
-- can be easier for the end user to consume than the original unfactored one. GroupBy type class
-- provides an interface to the factorize function which performs the factorization operation
-- and pFactor prints the factor in the pretty printed form.

type Factor b c = [(b,Rec c,Double)]

class (SumOut a b,Reduce a c,SubDim a b) => GroupBy a b c | a -> b c where
  factorize :: (Ord a,Ord b,Ord c) => Rec a -> Factor b c
  factorize xs = zipWith (\x y -> (fst x,reduce y,snd x)) (h xs) (k xs)
      where h = sort.fromRec.sumOut
            k = map mkRec.groupBy g.sortBy (compare `on` f).fromRec
            g x y = f x == f y
            f = proj.fst


pFact :: (Show a,Show b) => Factor a b -> IO ()
pFact = mapM_ pFactH
  where
    pFactH (b,a,v) = putStrLn $ show b ++ " : " ++ printf "%.3f" v ++ " (" ++ show a ++ ") \n"


instance Ord a => GroupBy (a,b) a b
instance Ord b => GroupBy (a,b) b a

instance Ord a => GroupBy (a,b,c) a (b,c)
instance Ord b => GroupBy (a,b,c) b (a,c)
instance Ord c => GroupBy (a,b,c) c (a,b)

instance Ord a => GroupBy (a,b,c,d) a (b,c,d)
instance Ord b => GroupBy (a,b,c,d) b (a,c,d)
instance Ord c => GroupBy (a,b,c,d) c (a,b,d)
instance Ord d => GroupBy (a,b,c,d) d (a,b,c)
