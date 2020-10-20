{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module StabilityHelper where

import qualified Data.Map as M 
import Data.Tuple.OneTuple (OneTuple(..))
import qualified Data.List as L 

import Record
import Info
import Valuation hiding (val)
import MDS
import Transformation
import Dimension

mode :: Bool 
mode = False   

-- Some type snynonyms
type Info2 o a b = (Info o a,Info a b)
type Val2 o a b  = (Val o a,Val a b)

type Info3 o a b c = (Info o a,Info a b,Info b c)
type Val3  o a b c = (Val o a,Val a b,Val b c)

type Info4 o a b c d = (Info o a,Info a b,Info b c,Info c d)
type Val4  o a b c d = (Val o a,Val a b,Val b c,Val c d)

-- Some commonly used constraint synonyms 
type Set2 a b = (Set a, Set b)
type Set3 a b c = (Set a,Set b,Set c)
type Set4 a b c d = (Set a,Set b,Set c,Set d)

type Valence2 a b = (Valence a,Valence b)
type Valence3 a b c = (Valence a,Valence b,Valence c)
type Valence4 a b c d = (Valence a,Valence b,Valence c,Valence d)

type SetVal a = (Set a,Valence a)
type SetVal2 a b = (Set2 a b,Valence2 a b)
type SetVal3 a b c = (Set3 a b c,Valence3 a b c)
type SetVal4 a b c d = (Set4 a b c d,Valence4 a b c d)

type Ord2 a b = (Ord a,Ord b)
type AHP3 a b c = (SetVal3 a b c,Covers (b,c) c)

-- valuation of multiple Info in a tuple.
-- this is needed in sensitivity analysis
class Valtuple a where
    valtuple :: a -> a 

instance (Ord o,SetVal2 a b) => Valtuple (Info2 o a b) where
    valtuple (x,y) = (valuation x,valuation y)

instance (Ord o,SetVal3 a b c) => Valtuple (Info3 o a b c) where
    valtuple (x,y,z) = (valuation x,valuation y,valuation z)

instance (Ord o,SetVal4 a b c d) => Valtuple (Info4 o a b c d) where
    valtuple (w,x,y,z) = (valuation w,valuation x,valuation y,valuation z)
-- ================================================================================
-- ================= HELPER FUNCTIONS FOR SENSITIVITY ANALYSIS ====================

-- difference in the component value between two objects
-- for a given component
cvdiff :: (Ord o,Ord a) => (o,o) -> Val o a -> a -> Double  
cvdiff (o1,o2) v a = lookupInfo (o1,a) v - lookupInfo (o2,a) v

-- difference in the priorities of the two objects o1 and o2 
pdiff :: (Ord o,Ord a) => (o,o) -> Val o a -> Double
pdiff (o1,o2) = (\x -> lookupRec o1 x - lookupRec o2 x) . total 

-- get all values corresponding to row o
rowVals :: (Ord o,Set a) => o -> Val o a -> [Double]
rowVals o v = map (\a -> lookupInfo (o,a) v) members 

-- get all the values corresponding to column b 
colVals :: (Set a,Ord b) => b -> Val a b -> [Double]
colVals b v = map (\a -> lookupInfo (a,b) v) members 

percent :: Double -> Double -> Percent
percent s v = (v/s)*100

-- dot product
dot :: [Double] -> [Double] -> Double
dot xs = sum.zipWith (*) xs 

-- compute dot product of row o of val o a with column b of val a b 
dotprod :: (Ord o,Set a,Ord b) => o -> b -> (Val o a,Val a b) -> Double
dotprod o b (v1,v2) = rowVals o v1 `dot` colVals b v2

-- denormalization is the opposite of normalization, that is going
-- from normalized to the original values 
denormalize :: (Set a, Set b,Valence b) => Info a b -> ((a,b),Maybe Double) -> ((a,b),Maybe Double)
denormalize i p@(_,Nothing) = p 
denormalize i (ab@(a,b),Just n)
    | valence b = (ab,Just $ n * sum avals)
    | otherwise = (ab,Just $ xn - ((1-theta)/(theta * rsum)))  
    where
        avals = colVals b i 
        theta = (lookupInfo ab (valuation i)-n)/(1-n)
        -- sum of reciprocals 
        sumreci= sum.map (\x -> 1/x)   
        rsum = sumreci $ map (\m -> lookupInfo (m,b) i) (members L.\\ [a])
        xn = lookupInfo (a,b) i 

-- ================================================================================
-- ================= HELPER FUNCTIONS COMPUTING CHANGE IN VALUES ==================

-- calculating the change required for the first level in the two level AHP
change21 :: (Ord o ,Set o,SetVal2 a b) => Val2 o a b -> (o,o) -> a -> ((o,a),Maybe Double)
change21 (v1,v2) o@(o1,o2) k  
    | deltaValP <= 100 = if mode then (p,Just deltaValP) else (p,Just deltaVal)  
    | otherwise = (p,Nothing)    
    where
        p = (o1,k)
        prioritydiff = pdiff o $ mkOneTuple v1 `extendBy` v2 
        weight_k  = lookupInfo (k,head members) v2
        value_ik  = lookupInfo (o1,k) v1
        valDiff_k = cvdiff o v1 k 
        
        deltaVal  = (prioritydiff/(prioritydiff + weight_k*(1-valDiff_k)))  
        deltaValP = percent value_ik deltaVal


-- calculating the change required for the second level in the two level AHP
change22 :: (Ord o,SetVal2 a b) => Val2 o a b -> (o,o) -> a -> ((a,b),Maybe Double)
change22 (v1,v2) o@(o1,o2) k  
    | deltaWgtP <= 100 = if mode then (p,Just deltaWgtP) else (p,Just deltaWgt)  
    | otherwise = (p,Nothing)    
    where
        p = (k,head members)
        prioritydiff = pdiff o $ mkOneTuple v1 `extendBy` v2 
        weight_k  = lookupInfo (k,head members) v2
        valDiff_k = cvdiff o v1 k 
        
        deltaWgt  = prioritydiff/valDiff_k
        deltaWgtP = percent weight_k deltaWgt 


-- calculating the change required for the second level in the three level AHP
change32 :: forall o a b c. (Ord o,AHP3 c a b) => Val3 o a b c -> (o,o) -> (a,b) -> ((a,b),Maybe Double)
change32 (v1,v2,v3) o@(o1,o2) ab@(a,b)  
    | cond1     = if mode then (ab,Just delta) else (ab,Just delta')
    | otherwise = (ab,Nothing)
    where 
        v12 = (mkOneTuple v1 `extendBy` v2) :: Val o (a,b)
        prioritydiff = pdiff o ((v12 `extendBy` v3) :: Val o (a,b,c)) 
        weightk = lookupInfo (b,head members) v3 
        valuebik= lookupInfo ab v2
        valdiffa= cvdiff o v1 a 
        odot1 = dotprod o1 b (v1,v2)
        odot2 = dotprod o2 b (v1,v2)
        
        delta'= prioritydiff/(prioritydiff + weightk * (valdiffa - odot1 + odot2)) 
        delta = delta' * 100/valuebik   
        
        cond1 = delta' <= valuebik
        --cond1 = bik-1 <= delta' && delta' <= bik 
