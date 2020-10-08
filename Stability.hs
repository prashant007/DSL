{-# LANGUAGE DeriveAnyClass,FlexibleContexts,ScopedTypeVariables#-}
module Stability where

import qualified Data.Map as M 
import Data.Tuple.OneTuple (OneTuple(..))
import qualified Data.List as L 

import Record
import Info
import Valuation
import MDS
import Focus
import Transformation
import Classes

type Pair o = (o,o)
data Sens o a = Sens {unpair :: Pair o,unsens :: M.Map a (Maybe Double)} 

mkSens :: Ord a => Pair o -> [(a,Maybe Double)] -> Sens o a 
mkSens p = Sens p . M.fromList 

fromSens :: Sens o a -> (Pair o,[(a,Maybe Double)])
fromSens x = (unpair x,M.toList . unsens $ x)

showPairM :: Show a => Int -> String -> (a,Maybe Double) -> String
showPairM n s (x,Just y') = showPairD 4 "" (x,y')
showPairM _ _ (x,_)       = show x ++ " -> *"

-- show record values as percentages
showSens :: (Show o,Show a) => Int -> String -> Sens o a -> String 
showSens n s p = (show . fst) p' ++ " : " ++ (showSet . map (showPairM n s) . snd) p'
    where p' = fromSens p 

instance (Show o,Show a) => Show (Sens o a) where
  show = showSens 4 ""

type Level = Int 

-- difference in the component value between two objects
-- for a given component
cvdiff :: (Ord o,Ord a) => (o,o) -> Val o a -> a -> Double  
cvdiff (o1,o2) v a = lookupInfo (o1,a) v - lookupInfo (o2,a) v


-- difference in the priorities of the two objects o1 and o2 
pdiff :: (Ord o,Ord a) => (o,o) -> Val o a -> Double
pdiff (o1,o2) = (\x -> lookupRec o1 x - lookupRec o2 x) . total 


-- ============= Sensitivity analysis for an AHP of two levels =====================================

change :: (Ord o,Ord a,Set b,Valence b) => Level -> (Val o a,Val a b) -> (o,o) -> a -> (a,Maybe Double)
change l (v1,v2) o@(o1,o2) k  
    | l == 1 && cond1 = (k,Just delta1) -- (k,Just delta1')  
    | l == 2 && cond2 = (k,Just delta2) -- (k,Just (pd/kdiff))  
    | otherwise = (k,Nothing)    
    where
        pd   = pdiff o $ mkOneTuple v1 `extendBy` v2
        wk   = lookupInfo (k,head members) v2
        aik  = lookupInfo (o1,k) v1
        kdiff= cvdiff o v1 k 
        
        delta1'= (pd/(pd + wk*(1-kdiff)))
        delta1 = delta1'*100/aik
        delta2 = (pd/kdiff)*(100/wk) 

        cond2 = delta2 <= 100
        cond1 = delta1 <= 100
        -- cond1 = aik-1 <= delta1' && delta1' <= aik 


sens2 :: (Ord o,Set a,Set b,Valence b) => Level -> (Val o a,Val a b) -> (o,o) -> Sens o a
sens2 l v o = mkSens o . map (change l v o) $ members 

-- ============= Sensitivity analysis for an AHP of three levels =====================================

-- first level of three level AHP problem

sens31 :: (Ord o,Set a,Set b,Set c,Valence a,Valence b,Valence c,SubDim (b,c) c) =>
          (Val o a,Val a b,Val b c) -> (o,o) -> Sens o a 
sens31 (v1,v2,v3) = let v' = mkOneTuple v2 `extendBy` v3 in sens2 1 (v1, projInfo v') 

-- second level of three level AHP problem

sens32 :: (Ord o,Set a,Set b,Set c,Valence b,Valence c,SubDim (a,b) b) =>
          (Val o a,Val a b,Val b c) -> (o,o) -> Sens o (a,b) 
sens32 v o = mkSens o. map (change' v o) $ ls 
    where ls = [(x,y) | x <- members, y <- members] 


-- helper functions for sens32

-- dot product
dot :: [Double] -> [Double] -> Double
dot xs = sum.zipWith (*) xs 

-- get all values corresponding to row o
rowVals :: (Ord o,Set a) => o -> Val o a -> [Double]
rowVals o v = map (\a -> lookupInfo (o,a) v) members 

colVals :: (Set a,Ord b) => b -> Val a b -> [Double]
colVals b v = map (\a -> lookupInfo (a,b) v) members 

computeDot :: (Ord o,Set a,Ord b) => o -> b -> (Val o a,Val a b) -> Double
computeDot o b (v1,v2) = rowVals o v1 `dot` colVals b v2

change' :: (Ord o,Set a,Set b,Set c,Valence b,Valence c,SubDim (a,b) b) => 
          (Val o a,Val a b,Val b c) -> (o,o) -> (a,b) -> ((a,b),Maybe Double)
change' (v1,v2,v3) o@(o1,o2) ab@(a,b) 
    | cond1      = (ab,Just delta)
    | otherwise  = (ab,Nothing)
    where
        v12 = mkOneTuple v1 `extendBy` v2
        pd  = pdiff o $ v12 `extendBy` v3 
        wk  = lookupInfo (b,head members) v3 
        bik = lookupInfo ab v2
        adiff = cvdiff o v1 a 
        odot1 = computeDot o1 b (v1,v2)
        odot2 = computeDot o2 b (v1,v2)
        
        delta'= pd / (pd + wk * (adiff - odot1 + odot2)) 
        delta = delta' * 100/bik   
        
        cond1 = delta' <= bik
        -- cond1 = bik-1 <= delta' && delta' <= bik 
            

-- third level of three level AHP problem
sens33 :: (Ord o,Ord a,Set b,Set c,Valence c,Valence b,SubDim (a,b) b) =>
          (Val o a,Val a b,Val b c) -> (o,o) -> Sens o b
sens33 (v1,v2,v3) = let v' = mkOneTuple v1 `extendBy` v2 in sens2 2 (projInfo v',v3) 

---- =====================================================================
data SType a b c d = S1 a | S2 (a,b) | S3 (b,c) | S4 c  


sens41 :: (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,
          Valence d,SubDim (b,c) c,SubDim (c,d) d) =>
          (Val o a,Val a b,Val b c,Val c d) -> (o,o) -> Sens o a 
sens41 (v1,v2,v3,v4) = let v2' = mkOneTuple v2 `extendBy` v3 in sens31 (v1,projInfo v2',v4)


sens42 :: (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,
          Valence d,SubDim (a,b) b,SubDim (c,d) d) =>
          (Val o a,Val a b,Val b c,Val c d) -> (o,o) -> Sens o (a,b) 
sens42 (v1,v2,v3,v4) = let v3' = mkOneTuple v3 `extendBy` v4 in sens32 (v1,v2,projInfo v3')


sens43 :: (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,
          Valence d,SubDim (a,b) b,SubDim (b,c) c) =>
          (Val o a,Val a b,Val b c,Val c d) -> (o,o) -> Sens o (b,c) 
sens43 (v1,v2,v3,v4) = let v1' = mkOneTuple v1 `extendBy` v2 in sens32 (projInfo v1',v3,v4)


sens44 :: (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,
           Valence d,SubDim (b,c) c,SubDim (a,c) c) =>
          (Val o a,Val a b,Val b c,Val c d) -> (o,o) -> Sens o c 
sens44 (v1,v2,v3,v4) = let v2' = mkOneTuple v2 `extendBy` v3 in sens33 (v1,projInfo v2',v4)

-- sens41 :: (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,
--           Valence d,SubDim (b,c) c,SubDim (c,d) d) =>
--           (Val o a,Val a b,Val b c,Val c d) -> (o,o) -> Sens o a 
-- sens41 (v1,v2,v3,v4) = let v2' = mkOneTuple v2 `extendBy` v3 in sens31 (v1,projInfo v2',v4)


-- sens42 :: (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,
--           Valence d,SubDim (a,b) b,SubDim (c,d) d) =>
--           (Val o a,Val a b,Val b c,Val c d) -> (o,o) -> Sens o (a,b) 
-- sens42 (v1,v2,v3,v4) = let v3' = mkOneTuple v3 `extendBy` v4 in sens32 (v1,v2,projInfo v3')


-- sens43 :: (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,
--           Valence d,SubDim (a,b) b,SubDim (b,c) c) =>
--           (Val o a,Val a b,Val b c,Val c d) -> (o,o) -> Sens o (b,c) 
-- sens43 (v1,v2,v3,v4) = let v1' = mkOneTuple v1 `extendBy` v2 in sens32 (projInfo v1',v3,v4)


-- sens44 :: (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,
--            Valence d,SubDim (b,c) c,SubDim (a,c) c) =>
--           (Val o a,Val a b,Val b c,Val c d) -> (o,o) -> Sens o c 
-- sens44 (v1,v2,v3,v4) = let v2' = mkOneTuple v2 `extendBy` v3 in sens33 (v1,projInfo v2',v4)


-- =======================================================================

allSens :: (Set o,Set a,Set b,Valence b) => Level -> (Val o a,Val a b) -> [Sens o a]
allSens l v 
    | l==1 = map (sens2 1 v) (genPairs (/=))
    | otherwise = map (sens2 2 v) (genPairs (<))

genPairs :: Set o => (o -> o -> Bool) ->  [(o,o)]
genPairs f = [(x,y) | x <- members, y <- members, f x y]
        
pSens :: (Show o,Show a) => [Sens o a]-> IO ()
pSens = mapM_ (\x -> putStrLn $ show x ++ "\n") 


