{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

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
import Valuation
import Factor
import Dimension
import MDS hiding (compare)


-- ================== GENERALIZE ==========================================
-- ========================================================================

-- The MDS explanations in general consist of explanations from various level.
-- For example, in the Car example the decision to buy a car is comprised of
-- features of the car like fuel and safety ratings as well as the people
-- advising the buyer namely the friends, and experts. These two levels are
-- independent of each other in the mind of an explanation consumer. The
-- generalize function provides an explanation in
-- terms of one level at a time.

-- generalize :: (Ord b,Split a b c) => ValDiff a -> Explain b
generalize :: (Ord a,Ord b,Covers a b) => Rec a -> Analysis b
generalize = analyze . focus

-- ================== Selector ==========================================
-- ========================================================================

filter :: (a -> Bool) -> Info o a -> Info o a
filter f = onInfo (M.map (subRec f))

-- only :: (Eq b,Split a b c) => b -> Info o a -> Info o a
only :: (Eq b,Covers a b) => b -> Info o a -> Info o a
only v = filter $ (==v) . project

-- except :: (Eq b,Split a b c) => b -> Info o a -> Info o a
except :: (Eq b,Covers a b) => b -> Info o a -> Info o a
except v = filter $ (/=v) . project

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

projectRec :: (Covers a b,Ord a,Ord b) => Rec a -> Rec b
projectRec x = M.foldrWithKey iterRec emptyRec (groupRecBy project x)
  where
    iterRec :: Ord b => b -> Rec a -> Rec b -> Rec b
    iterRec b m = insertRec b (sumRec m)


projectInfo :: (Ord a, Ord b, Covers a b) => Info o a -> Info o b 
projectInfo = mapInfo projectRec

-- ===================== FOCUS =============================================
-- ==========================================================================

class Focus a b where
    focus :: a -> b

instance (Covers a b,Ord a,Ord b) => Focus (Rec a) (Rec b) where
    focus = projectRec

instance (Covers a b,Ord a,Ord b) => Focus (Val o a) (Val o b) where
    focus = mapInfo focus

-- ================== FACTORIZING EXPLANATIONS ===============================
-- ===========================================================================

-- factor :: (Ord d,Ord a,Ord d',Covers d a,Shrink d d') => Rec d -> Factor a d'
factor :: (Ord d,Ord a,Ord d',Split d a d') => Rec d -> Factor a d'
factor xs = formatFactor . Factor $ zipMap mkFact (unRec . projectRec $ xs)
                                                   (groupRecBy project xs)
    where mkFact = \_ x y -> (x,shrinkRec y)

impact :: Ord a => Rec a -> Factor a ()
impact r = factor (mkOneTupleRec r)

-- ========== VALUE DIFFERENCE IMPACTS (VDI) =================================
-- ===========================================================================

vdi :: Ord a => a -> a -> Factor a b -> Factor a b -> (Percent,Percent)
vdi a b f1 f2 = (compImpact a b f1,compImpact a b f2)
  where
    -- compare impacts
    compImpact :: Ord a => a -> a -> Factor a b -> Percent
    compImpact x y f = (lookUpFactor x f/lookUpFactor y f)*100

pvdi :: (Percent,Percent) -> IO ()
pvdi (x,y) = do
  let showP a = printf "%.0f" a ++ "%"
  putStrLn $ "(" ++ showP x ++ ", " ++ showP y ++ ")"


