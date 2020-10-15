{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,KindSignatures,GADTs,DataKinds,TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass,FlexibleContexts,ScopedTypeVariables,UndecidableInstances #-}
module Stability where

import qualified Data.Map as M 
import Data.Tuple.OneTuple (OneTuple(..))
import qualified Data.List as L 
import GHC.TypeLits

import Record
import Info
import Valuation hiding (val)
import MDS
import Focus
import Transformation
import Dimension

type Pair o = (o,o)


mode :: Bool 
mode = False     

data Sens (l::Nat) o a where
     Sens :: Pair o -> M.Map a (Maybe Double) -> Sens l o a


carTuple = (carFeatures,featureOpinions,weights)  
c11 = sens carTuple (Honda,BMW) :: Sens (Car,Feature) 

*Main> c11
(Honda,BMW) : {(Honda,Price) -> -1177.9528, (Honda,MPG) -> 1.1098, (Honda,Safety) -> 0.6765}

c2  = sens carTuple (Honda,BMW) :: Sens (Feature,Opinion)
*Main> c2
(Honda,BMW) : {(Price,Personal) -> 4.1851, (Price,Expert) -> *,
 (MPG,Personal) -> -3.6824, (MPG,Expert) -> -12.5564, (Safety,Personal) -> *, 
 (Safety,Expert) -> *}

featureOpinions :: Info Feature Opinion
featureOpinions = info [Price  --> [Personal --> (5- 4.1851), Expert --> 3],
                        MPG    --> [Personal --> 3, Expert --> 5],
                        Safety --> [Personal --> 2, Expert --> 2]]


unpair :: Sens l o a -> Pair o 
unpair (Sens p _) = p 

unsens :: Sens l o a -> M.Map a (Maybe Double) 
unsens (Sens _ m) = m 

mkSens :: Ord a => Pair o -> [(a,Maybe Double)] -> Sens l o a 
mkSens p = Sens p . M.fromList 

mapSens :: Ord b => ((a,Maybe Double) -> (b,Maybe Double)) -> Sens l o a -> Sens l o b 
mapSens f =  (\(x,y) -> mkSens x $ map f y) . fromSens

changeLevel :: Sens l o a -> Sens l' o a   
changeLevel s = Sens (unpair s) (unsens s)

fromSens :: Sens l o a -> (Pair o,[(a,Maybe Double)])
fromSens x = (unpair x,M.toList . unsens $ x)

-- show instance for Sens type
instance (Show o,Show a) => Show (Sens l o a) where
  show = showSens 4 ""

-- show record values as percentages
showSens :: (Show o,Show a) => Int -> String -> Sens l o a -> String 
showSens n s p = (show . fst) p' ++ " : " ++ (showSet . map (showPairM n s) . snd) p'
    where p' = fromSens p 

showPairM :: Show a => Int -> String -> (a,Maybe Double) -> String
showPairM n s (x,Just y') = showPairD n "" (x,y')
showPairM _ _ (x,_)       = show x ++ " -> *"
     
denormalize :: (Set a,Set b,Valence b) => Info a b -> ((a,b),Maybe Double) -> ((a,b),Maybe Double)
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
        


class Valtuple a where
    valtuple :: a -> a 

instance (Ord o,Set a,Valence a,Valence b,Set b) => Valtuple (Info o a, Info a b) where
    valtuple (x,y) = (valuation x,valuation y)

instance (Ord o,Set a,Valence a,Valence b,Valence c,Set b,Set c) => Valtuple (Info o a, Info a b,Info b c) where
    valtuple (x,y,z) = (valuation x,valuation y,valuation z)


class (Set b,Ord b,Set c,Covers a (Info b c),Valence c,Valtuple a) => 
      Sensitivity (l :: Nat) o a b c | l a -> b c where
    sensN :: a -> (o,o) -> Sens l o (b,c) 


sens  :: (Ord b,Sensitivity l o a b c) => Covers a (Info b c) => a -> (o,o) -> Sens l o (b,c) 
sens a = mapSens (denormalize (project a)) . sensN (valtuple a) 

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

-- ================================================================================
-- ========== Sensitivity Analysis For a 2 level AHP ==============================

type Info2 o a b = (Info o a,Info a b)
type Val2 o a b  = (Val o a,Val a b)


instance (Ord o,Set o,Set a,Set b,Valence a,Valence b) => Sensitivity 1 o (Info2 o a b) o a where
    sensN v o = mkSens o . map (change1 v o) $ members 


instance (Ord o,Set o,Set a,Set b,Valence a,Valence b) => Sensitivity 2 o (Info2 o a b) a b where
    sensN v o = mkSens o . map (change2 v o) $ members 

-- Helper functions for sensitivity analysis for a 2 level AHP

change1 :: (Ord o,Ord a,Set o,Set a,Set b,Valence a,Valence b) =>
           Val2 o a b -> (o,o) -> a -> ((o,a),Maybe Double)
change1 (v1,v2) o@(o1,o2) k  
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
change2 :: (Ord o,Ord a,Set o,Set a,Set b,Valence a,Valence b) => 
           Val2 o a b -> (o,o) -> a -> ((a,b),Maybe Double)
change2 (v1,v2) o@(o1,o2) k  
    | deltaWgtP <= 100 = if mode then (p,Just deltaWgtP) else (p,Just deltaWgt)  
    | otherwise = (p,Nothing)    
    where
        p = (k,head members)
        prioritydiff = pdiff o $ mkOneTuple v1 `extendBy` v2 
        weight_k  = lookupInfo (k,head members) v2
        valDiff_k = cvdiff o v1 k 
        
        deltaWgt  = prioritydiff/valDiff_k
        deltaWgtP = percent weight_k deltaWgt     


-- -- ================================================================================
-- -- ========== Sensitivity Analysis For a 3 level AHP ==============================

type Info3 o a b c = (Info o a,Info a b,Info b c)
type Val3  o a b c = (Val o a,Val a b,Val b c)

valuation3 :: (Ord o,Set a,Set b,Set c,Valence a,Valence b,Valence c) => 
              Info3 o a b c -> Val3 o a b c 
valuation3 (i1,i2,i3) = (valuation i1,valuation i2,valuation i3)


-- Sensitivity analysis of Level 1 of a 3 level AHP 
instance (Ord o,Ord b,Set o,Set a,Set b,Set c,Valence a,Valence b,Valence c,Covers (b,c) c) => 
         Sensitivity 1 o (Info3 o a b c) o a where
    sensN (v1,v2,v3) o = let v'= mkOneTuple v2 `extendBy` v3 :: Val a (b,c)
                         in sensN (v1,projInfo v') o :: Sens 1 o (o,a)      

-- -- ==============================================================================================

-- Sensitivity analysis of Level 2 of a 3 level AHP 
instance (Ord o,Ord b,Set b,Set a,Set c,Valence a,Valence c,Valence b,Covers (a,b) b) => 
         Sensitivity 2 o (Info3 o a b c) a b where
    sensN v o = let ls = [(x,y) | x <- members, y <- members] 
                in mkSens o. map (change' v o) $ ls 


-- helper functions for sens32
-- dot product
dot :: [Double] -> [Double] -> Double
dot xs = sum.zipWith (*) xs 

-- compute dot product of row o of val o a with column b of val a b 
dotprod :: (Ord o,Set a,Ord b) => o -> b -> (Val o a,Val a b) -> Double
dotprod o b (v1,v2) = rowVals o v1 `dot` colVals b v2


change' :: forall o a b c. (Ord o,Set a,Set b,Set c,Valence b,Valence c,Covers (a,b) b) => 
           Val3 o a b c -> (o,o) -> (a,b) -> ((a,b),Maybe Double)
change' (v1,v2,v3) o@(o1,o2) ab@(a,b)  
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

-- ==============================================================================================
-- Sensitivity analysis of Level 3 of a 3 level AHP 
instance (Show o,Show a,Show b,Show c,Ord o,Ord b,Set o,Set b,Set a,Set c,Valence a,Valence b,Valence c,Covers (a,b) b) => 
         Sensitivity 3 o (Info3 o a b c) b c where
    sensN (v1,v2,v3) o = let v'= mkOneTuple v1 `extendBy` v2 :: Val o (a,b)
                             s = sensN (projInfo v',v3) o :: Sens 2 o (b,c)
                         in changeLevel s :: Sens 3 o (b,c) 

-- -- ================================================================================
-- -- ========== Sensitivity Analysis For a 4 level AHP ==============================
-- type Val4 o a b c d = (Val o a,Val a b,Val b c,Val c d)

-- instance (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,Valence d,Covers (b,c) c) =>
--           Sensitivity 4 1 o (Val4 o a b c d) a where 
--     sens (v1,v2,v3,v4) o = let v2'= mkOneTuple v2 `extendBy` v3 :: Val a (b,c)  
--                                s  = sens (v1,projInfo v2',v4) o :: Sens 3 1 o a 
--                            in changeLevel s :: Sens 4 1 o a 

-- -- Sensitivity analysis of Level 2 of a 4 level AHP 
-- instance (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,Valence d,Covers (c,d) d) =>
--           Sensitivity 4 2 o (Val4 o a b c d) (a,b) where 
--     sens (v1,v2,v3,v4) o = let v3'= mkOneTuple v3 `extendBy` v4 :: Val b (c,d)  
--                                s  = sens (v1,v2,projInfo v3') o :: Sens 3 2 o (a,b) 
--                            in changeLevel s :: Sens 4 2 o (a,b) 


-- -- Sensitivity analysis of Level 3 of a 4 level AHP 
-- instance (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,Valence d,Covers (a,b) b) =>
--           Sensitivity 4 3 o (Val4 o a b c d) (b,c) where 
--     sens (v1,v2,v3,v4) o = let v1'= mkOneTuple v1 `extendBy` v2 :: Val o (a,b)  
--                                s  = sens (projInfo v1',v3,v4) o :: Sens 3 2 o (b,c) 
--                            in changeLevel s :: Sens 4 3 o (b,c) 


-- -- Sensitivity analysis of Level 4 of a 4 level AHP 
-- instance (Ord o,Set a,Set b,Set c,Set d,Valence a,Valence b,Valence c,Valence d,Covers (b,c) c) =>
--           Sensitivity 4 4 o (Val4 o a b c d) c where 
--     sens (v1,v2,v3,v4) o = let v2'= mkOneTuple v2 `extendBy` v3 :: Val a (b,c)  
--                                s  = sens (v1,projInfo v2',v4) o :: Sens 3 3 o c  
--                            in changeLevel s :: Sens 4 4 o c 

-- -- =======================================================================

type Level = Int 

-- allSens :: forall o a b. (Ord o,Set o,Set a,Set b,Valence a,Valence b) => 
--            Level -> (Val o a,Val a b) -> Either [Sens 1 o (o,a)] [Sens 2 o (a,b)] 
-- allSens l v 
--     | l==1 = Left $ map (\x -> (sens v x)::Sens 1 o (o,a)) (genPairs (/=))
--     | otherwise = Right $ map (\x -> (sens v x)::Sens 2 o (a,b)) (genPairs (<))

genPairs :: Set o => (o -> o -> Bool) ->  [(o,o)]
genPairs f = [(x,y) | x <- members, y <- members, f x y]
        

pSensE :: (Show o,Show a,Show b) => Either [Sens l o (o,a)] [Sens l' o (a,b)] -> IO ()
pSensE (Left x) = pSens x 
pSensE (Right x) = pSens x 

pSens :: (Show o,Show a) => [Sens l o a]-> IO ()
pSens = mapM_ (\x -> putStrLn $ show x ++ "\n") 


