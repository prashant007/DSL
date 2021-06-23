{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module SensitivityHelper where

import Data.Tuple.OneTuple (OneTuple(..))
import qualified Data.List as L 

import Record
import Info
import Valuation hiding (val)
import Dimension
import SensDataType 

-- mode true is percentage representation 
mode :: Bool 
mode = False   

-- Some type snynonyms
type Info2 o a b = (Info o a,Info a b)
type Val2 o a b  = (Val o a,Val a b)

type Info3 o a b c = (Info o a,Info a b,Info b c)
type Val3  o a b c = (Val o a,Val a b,Val b c)

type Info4 o a b c d = (Info o a,Info a b,Info b c,Info c d)
type Val4  o a b c d = (Val o a,Val a b,Val b c,Val c d)

type Info5 o a b c d e = (Info o a,Info a b,Info b c,Info c d,Info d e)

-- Some commonly used constraint synonyms 
type Set2 a b = (Set a, Set b)
type Set3 a b c = (Set a,Set b,Set c)
type Set4 a b c d = (Set a,Set b,Set c,Set d)

type Bound2 a b = (Bound a,Bound b)

type Valence2 a b = (Valence a,Valence b)
type Valence3 a b c = (Valence a,Valence b,Valence c)
type Valence4 a b c d = (Valence a,Valence b,Valence c,Valence d)

type SetVal a = (Set a,Valence a)
type SetVal2 a b = (Set2 a b,Valence2 a b)
type SetVal3 a b c = (Set3 a b c,Valence3 a b c)
type SetVal4 a b c d = (Set4 a b c d,Valence4 a b c d)

type Ord2 a b = (Ord a,Ord b)
type AHP3 a b c = (SetVal3 a b c,Covers (b,c) c)


class Norm a where
    norm :: a -> a 

instance (Ord o,SetVal2 a b) => Norm (Info2 o a b) where
    norm (x,y) = (valuation x,valuation y)

instance (Ord o,SetVal3 a b c) => Norm (Info3 o a b c)  where
    norm (x,y,z) = (valuation x,valuation y,valuation z)

instance (Ord o,SetVal4 a b c d) => Norm (Info4 o a b c d) where
    norm (w,x,y,z) = (valuation w,valuation x,valuation y,valuation z)

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

-- some times the change suggested by sensitivity analysis may not be feasible.
-- This function ensures that the changes suggested by sensitivity analysis
-- is within the acceptable limits for an attribute. 
checkBound :: Bound b => a -> b -> (Double,Double) -> (a,Maybe Double) 
checkBound a b (cv,ov) = if (cond1 || cond2) && cond3 then (a,Just cv) else (a,Nothing)
    where 
        u = fromIntegral . upperBound $ b  
        cond1 = cv >= 0 && cv < u 
        cond2 = cv < 0 && abs cv < u 
        cond3 = (ov - cv) > 0 && (ov - cv) <= u 


-- denormalization is the opposite of normalization, that is going
-- from normalized to the original values 
denormalize :: (Set a, SetVal b,Bound b) => Info a b -> b -> (a,Maybe Double) -> (a,Maybe Double)
denormalize i _ p@(a,Nothing) = p 
denormalize i b (a,Just n)
    | valence b = checkBound a b (n * sum avals,xn) 
    | otherwise = if rval' >= 1 then checkBound a b (rval,xn) else  (a,Nothing)
    where
        avals = colVals b i 
               
        -- sum of reciprocals 
        sumreci= sum.map (\x -> 1/x)   
        rsum = sumreci $ map (\m -> lookupInfo (m,b) i) (members L.\\ [a])
        xn = lookupInfo (a,b) i 
        
        rval' = ((1-theta)/(theta * rsum))
        rval  = xn - rval' 
        theta = (lookupInfo (a,b) (valuation i)-n)/(1-n)

-- ================================================================================
-- ================= HELPER FUNCTIONS COMPUTING CHANGE IN VALUES ==================

change21 :: (Ord o ,Set o,SetVal2 a b) => Val2 o a b -> (o,o) -> a -> o -> (o,Maybe Double)
change21 v (o1,o2) k o' 
    | o2 == o' = change21' v (o2,o1) o' k 
    | otherwise= change21' v (o1,o2) o' k 


-- calculating the change required for the first level in the two level AHP
change21' :: (Ord o ,Set o,SetVal2 a b) => Val2 o a b -> (o,o) -> o -> a -> (o,Maybe Double)
change21' (v1,v2) o o' k 
    | cond1 && condf deltaValP = retValue deltaValP deltaVal  
    | cond2 && condf deltaValP'= retValue deltaValP' deltaVal'  
    | otherwise = (o',Nothing)    
    where
        prioritydiff = pdiff o $ mkOneTuple v1 `extendBy` v2 
        weight_k  = lookupInfo (k,head members) v2
        value_ik  = lookupInfo (o',k) v1
        valDiff_k = cvdiff o v1 k 

        percentk x= percent value_ik x 
        retValue p x = if mode then (o',Just p) else (o',Just x)
        
        deltaVal  = (prioritydiff/(prioritydiff + weight_k*(1-valDiff_k)))  
        deltaValP = percentk deltaVal
        
        deltaVal' = prioritydiff/(prioritydiff - weight_k * valDiff_k)
        deltaValP'= percentk deltaVal'    

        cond1 = elem o' [fst o,snd o]
        cond2 = not cond1 
        condf x =  x <= 100  


--calculating the change required for the second level in the two level AHP
change22 :: (Ord o,SetVal2 a b) => Val2 o a b -> (o,o) -> a -> (a,Maybe Double)
change22 (v1,v2) o k
    | deltaWgtP <= 100 = if mode then (k,Just deltaWgtP) else (k,Just deltaWgt)  
    | otherwise = (k,Nothing)    
    where
        prioritydiff = pdiff o $ mkOneTuple v1 `extendBy` v2 
        weight_k  = lookupInfo (k,head members) v2
        valDiff_k = cvdiff o v1 k 
        
        deltaWgt  = prioritydiff/valDiff_k
        deltaWgtP = percent weight_k deltaWgt 


-- calculating the change required for the second level in the three level AHP
change32 :: forall o a b c. (Ord o,AHP3 c a b) => Val3 o a b c -> (o,o) -> (a,b) -> (a,Maybe Double)
change32 (v1,v2,v3) o@(o1,o2) ab@(a,b) 
    | cond1     = if mode then (a,Just delta) else (a,Just delta')
    | otherwise = (a,Nothing)
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
        --cond1 = valuebik-1 <= delta' && delta' <= valuebik 
