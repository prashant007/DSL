{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,InstanceSigs #-}

module Transformation where

import Data.Function
import qualified Data.Map as M 
import Text.Printf
import Data.List 

import MDS
import Attribute
import Object 
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

-- ================== SUMOUT ==============================================
-- ========================================================================

-- Sometimes we need to get rid of all but one elements in the tuples of Attr values
-- Consider the Attr val {(Friend,Fuel) -> 0.048,(Friend,Price) -> 0.032(Expert,Price) -> 0.080}. 
-- Suppose, we want to to get the attr val just in terms of the friends and experts, that
-- get rid of the feature component from the tuples. Once we get rid of (sum out) the second 
-- component of the tuple, the new tuple would look like this: 
-- {Friend -> 0.048,Friend -> 0.032,Expert -> 0.080} which can now be rewritten as 
-- {Friend -> 0.080,Expert -> 0.080}. Similarly, we could sum out the first component of the tuple
-- thereby giving us this attr val: {Fuel -> 0.048,Price -> 0.112}. SumOut is a type class
-- used to achieve this effect on attr values. 

class (Projector a b,Ord b) => SumOut a b | a -> b where
  sumOut :: Attr a -> Attr b 
  sumOut = mkAttr.map h.groupBy g.sortBy (compare `on` f).fromAttr
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


class (Ord o,Ord b) => Object a o b | a -> o b where
    getPair :: a -> (o,b)

    crtObj :: Attr a -> Obj o b 
    crtObj = mkObj.l.map h.groupBy g.sortBy (compare `on` f).fromAttr
        where l = map l2.groupBy l1  
              l1 x y = fst x == fst y
              l2 x = (fst.head $ x,mkAttr.map snd $ x)
              h x = (h' fst x,(h' snd x,sum.map snd $ x))
              h' k x = k.f.head $ x   
              g x y = f x == f y
              f = getPair.fst 


instance (Ord a,Ord b) => Object (a,b,c,d) a b where
  getPair (a,b,c,d) = (a,b)

instance (Ord b,Ord c) => Object (a,b,c,d) b c where
  getPair (a,b,c,d) = (b,c)

instance (Ord c,Ord d) => Object (a,b,c,d) c d where
  getPair (a,b,c,d) = (c,d)

-- ===================== REDUCE =============================================
-- ==========================================================================

-- Many a times the Attr value may have an argument that stays the same in all 
-- the elements of an Attr value. The Reduce type class provides a way, using
-- the denoise function to achieve this.

class Reduce a b | a -> b where
  rmv :: a -> b 

  reduce :: (Ord a,Ord b) => Attr a -> Attr b 
  reduce = mkAttr.map (\(x,n) -> (rmv x,n)).fromAttr

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

-- Consider the following Attr value: {(Friend,Safety) -> -0.048,(Expert,Safety) -> -0.064}
-- We observe that Safety could be factored in this Attr value giving us this pretty printed
-- representation: Safety: -0.112 (Friend:-0.048, Expert:-0.064). This factored representation
-- can be easier for the end user to consume than the original unfactored one. GroupBy type class
-- provides an interface to the factorize function which performs the factorization operation
-- and pFactor prints the factor in the pretty printed form. 

type Factor b c = [(b,Attr c,Double)]

class (SumOut a b,Reduce a c,Projector a b) => GroupBy a b c | a -> b c where 
  factorize :: (Ord a,Ord b,Ord c) => Attr a -> Factor b c
  factorize xs = zipWith (\x y -> (fst x,reduce y,snd x)) (h xs) (k xs)
      where h = sort.fromAttr.sumOut
            k = map mkAttr.groupBy g.sortBy (compare `on` f).fromAttr 
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

