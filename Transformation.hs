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
import Focus
import Classes
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
generalize = explain.projRec

-- ================== Selector ==========================================
-- ========================================================================

filter :: (a -> Bool) -> Info o a -> Info o a
filter f = onInfo (M.map (subRec f))

-- only :: (Eq b,Split a b c) => b -> Info o a -> Info o a
only :: (Eq b,SubDim a b) => b -> Info o a -> Info o a
only v = filter $ (==v) . proj

-- except :: (Eq b,Split a b c) => b -> Info o a -> Info o a
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

projRec :: (SubDim a b,Ord a,Ord b) => Rec a -> Rec b
projRec x = M.foldrWithKey iterRec emptyRec (groupRecBy proj x)
  where
    iterRec :: Ord b => b -> Rec a -> Rec b -> Rec b
    iterRec b m = insertRec b (sumRec m)

-- ===================== REDUCE =============================================
-- ==========================================================================

-- Many a times the record value may have an argument that stays the same in all
-- the elements of an Norm value. The Reduce type class provides a way, using
-- the denoise function to achieve this.
reduce :: (Ord a,Ord b,Reduce a b) => Rec a -> Rec b
reduce = onRec (M.mapKeys reducedTup)

-- ================== FACTORIZING EXPLANATIONS ===============================
-- ===========================================================================

factorize :: (Ord a,Ord b,Ord c,SubDim a b,Reduce a c) => Rec a -> Focus b c
factorize xs = formatFocus . Focus $ zipMap mkFact (unRec . projRec $ xs)
                                                   (groupRecBy proj xs)
    where mkFact = \_ x y -> (x,reduce y)

impact :: Ord a => Rec a -> Focus a ()
impact = factorize . mkOneTupleRec

-- ========== VALUE DIFFERENCE IMPACTS (VDI) =================================
-- ===========================================================================

vdi :: Ord a => a -> a -> Focus a b -> Focus a b -> (Percent,Percent)
vdi a b f1 f2 = (compImpact a b f1,compImpact a b f2)
  where
    -- compare impacts
    compImpact :: Ord a => a -> a -> Focus a b -> Percent
    compImpact x y f = (lookUpFocus x f/lookUpFocus y f)*100

pvdi :: (Percent,Percent) -> IO ()
pvdi (x,y) = do
  let showP a = printf "%.0f" a ++ "%"
  putStrLn $ "(" ++ showP x ++ ", " ++ showP y ++ ")"
