{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,InstanceSigs,NamedWildCards,PartialTypeSignatures #-}

module Transformation where

import Data.Function
import qualified Data.Map as M
import Text.Printf
import Data.List hiding (filter)
import Prelude hiding (filter)
import Data.Tuple.OneTuple as T (only,OneTuple) 
import Data.Maybe 
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

-- generalize :: (Ord b,Split a b c) => ValDiff a -> Explain b
generalize :: (Ord a,Ord b,SubDim a b) => ValDiff a -> Explain b
generalize = explain.sumOut

-- ================== Selector ==========================================
-- ========================================================================

filter :: (a -> Bool) -> Info o a -> Info o a
filter f = onInfo (M.map (subRec f))

-- only :: (Eq b,Split a b c) => b -> Info o a -> Info o a
only :: (Eq b,SubDim a b) => b -> Info o a -> Info o a
only v = filter $ (==v) . focus 

-- except :: (Eq b,Split a b c) => b -> Info o a -> Info o a
except :: (Eq b,SubDim a b) => b -> Info o a -> Info o a
except v = filter $ (/=v) . focus 

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

fstFocus :: SubDim a c =>  (a,b) -> c 
fstFocus = (focus.fst)

-- sumOut :: (Split a b c,Ord b) => Rec a -> Rec b
sumOut :: (SubDim a b,Ord a,Ord b) => Rec a -> Rec b
sumOut =  mkRec . foldRec (map mkPair . sortNGroupBy fstFocus)
  where mkPair xs@((x,_):_) = (focus x,sum.map snd $ xs)
      

-- ===================== REDUCE =============================================
-- ==========================================================================

-- Many a times the record value may have an argument that stays the same in all
-- the elements of an Norm value. The Reduce type class provides a way, using
-- the denoise function to achieve this.


-- reduce :: (Ord a,Ord b,Split a c b) => Rec a -> Rec b
reduce :: (Ord a,Ord b,Reduce a b) => Rec a -> Rec b
reduce = onRec (M.mapKeys remainder)

-- ========================== FACTORING =====================================
-- ==========================================================================

-- Consider the following record value: {(Friend,Safety) -> -0.048,(Expert,Safety) -> -0.064}
-- We observe that Safety could be factored in this Norm value giving us this pretty printed
-- representation: Safety: -0.112 (Friend:-0.048, Expert:-0.064). This factored representation
-- can be easier for the end user to consume than the original unfactored one. GroupBy type class
-- provides an interface to the factorize function which performs the factorization operation
-- and pFactor prints the factor in the pretty printed form.

type Factor b c = [(b,Rec c,Double)]

-- this changes factor values from absolutes values to percentages 
formatFact :: Ord c => Factor b c -> Factor b c 
formatFact ls = map percentFact ls   
  where
    sumF = sum.map (\(x,y,z) -> abs z) $ ls
    mkPercent s v = ((abs v)/s)*100
    percentFact = \(x,y,z) -> (x,percentRec y,mkPercent sumF z)

    percentRec :: Ord a => Rec a -> Rec a 
    percentRec x = let sumR = foldRec (sum.map (abs.snd)) $ x
                   in mapRec (mkPercent sumR) x  


-- factorize :: (Ord a,Ord b,Ord c,Split a b c) => Rec a -> Factor b constituents
factorize :: (Ord a,Ord b,Ord c,SubDim a b,Reduce a c) => Rec a -> Factor b c
factorize xs = formatFact $ zipWith mkFact (attrScore xs) (groupRecBy focus xs)
    where mkFact = \x y -> (fst x,reduce y,snd x)
          -- impact/score of individual attributes 
          attrScore = foldRec sort . sumOut          


pFact :: (Show a,Show b) => Factor a b -> IO ()
pFact = mapM_ pFactH 
  where
    pFactH :: (Show b,Show c) => (b,Rec c,Double) -> IO () 
    pFactH (b,c,v) = putStrLn $ show b ++ " : " ++ printf "%.0f" v ++ " %\n " ++ showRec c ++ " \n"
    
    showRec :: Show a => Rec a -> String
    showRec = showSetLn . foldRec (map showPairP) 

    showPairP :: Show a => (a,Double) -> String
    showPairP (x,y) = show x ++ " -> " ++ printf "%.0f" y ++ " %"

    showSetLn :: [String] -> String
    showSetLn xs = "\t{" ++ intercalate ",\n\t " xs ++ "}"


type Percent = Double 

vdi :: Eq a => a -> a -> Factor a b -> Factor a b -> (Percent,Percent) 
vdi a b f1 f2 = (compImpact a b f1,compImpact a b f2)
  where
    lookUpAttr :: Eq a => a -> Factor a b -> Double
    lookUpAttr x  = fromJust . lookup x . map (\(x,y,z) -> (x,z)) 

    -- compare impacts 
    compImpact :: Eq a => a -> a -> Factor a b -> Percent 
    compImpact x y f = (lookUpAttr x f/lookUpAttr y f)*100

pvdi :: (Percent,Percent) -> IO ()
pvdi (x,y) = do
  let showP a = printf "%.0f" a ++ "%" 
  putStrLn $ "(" ++ showP x ++ ", " ++ showP y ++ ")" 