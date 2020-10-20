{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,DataKinds#-}
{-# LANGUAGE FlexibleContexts,ScopedTypeVariables,UndecidableInstances,ConstraintKinds #-}

module Stability where

import qualified Data.Map as M 
import Data.Tuple.OneTuple (OneTuple(..))
import qualified Data.List as L 
import GHC.TypeLits

import Record
import Info
import Valuation hiding (val)
import MDS
import Transformation
import Dimension
import Sens 
import StabilityHelper


class (Ord b,Set b, Set c,Covers a (Info b c),Valence c,Valtuple a) => Sval o a b c | a -> b c where
    sens' :: a -> (o,o) -> Sens o (b,c) 

-- maybe add percentages here
sens  :: (Ord b,Sval o a b c,Covers a (Info b c)) => a -> (o,o) -> Sens o (b,c) 
sens a = mapSens (denormalize (project a)) . sens' (valtuple a) 

-- ================================================================================
-- ========== Sensitivity Analysis For a 2 level AHP ==============================

-- Sensitivity analysis of Level 1 of a 2 level AHP 
instance (Ord o,Set o,SetVal2 a b) => Sval o (Info2 o a b) o a where
    sens' v o = mkSens o . map (change21 v o) $ members 

-- Sensitivity analysis of Level 2 of a 2 level AHP 
instance (Ord o,Set o,SetVal2 a b) => Sval o (Info2 o a b) a b where
    sens' v o = mkSens o . map (change22 v o) $ members 

-- -- ================================================================================
-- -- ========== Sensitivity Analysis For a 3 level AHP ==============================

-- Sensitivity analysis of Level 1 of a 3 level AHP 
instance (Set o,Ord2 o b,AHP3 a b c) => Sval o (Info3 o a b c) o a where
    sens' (v1,v2,v3) = let v'= mkOneTuple v2 `extendBy` v3 :: Val a (b,c)
                       in sens' (v1,projectInfo v') 



-- Sensitivity analysis of Level 2 of a 3 level AHP 
instance (Ord2 o b,Valence3 a b c,AHP3 c a b) => Sval o (Info3 o a b c) a b where
    sens' v o = let ls = [(x,y) | x <- members, y <- members] 
                in mkSens o. map (change32 v o) $ ls 


-- Sensitivity analysis of Level 3 of a 3 level AHP 
instance (Set o,Ord2 o b,AHP3 c a b) => Sval o (Info3 o a b c) b c where
    sens' (v1,v2,v3)= let v'= mkOneTuple v1 `extendBy` v2 :: Val o (a,b)
                      in sens' (projectInfo v',v3) 

-- -- ================================================================================
-- -- ========== Sensitivity Analysis For a 4 level AHP ==============================

-- Sensitivity analysis of Level 1 of a 4 level AHP     
instance (Ord o,Set o,SetVal d,AHP3 a b c) => Sval o (Info4 o a b c d) o a where 
    sens' (v1,v2,v3,v4) = let v2' = mkOneTuple v2 `extendBy` v3 :: Val a (b,c)  
                          in sens' (v1,projectInfo v2',v4)

-- Sensitivity analysis of Level 2 of a 4 level AHP 
instance (Ord o,SetVal a,AHP3 b c d) => Sval o (Info4 o a b c d) a b where 
    sens' (v1,v2,v3,v4) = let v3'= mkOneTuple v3 `extendBy` v4 :: Val b (c,d)  
                          in sens' (v1,v2,projectInfo v3')

-- Sensitivity analysis of Level 3 of a 4 level AHP 
instance (Ord o,SetVal c,AHP3 d a b) => Sval o (Info4 o a b c d) b c where 
    sens' (v1,v2,v3,v4) = let v1'= mkOneTuple v1 `extendBy` v2 :: Val o (a,b)  
                          in sens' (projectInfo v1',v3,v4)  

-- Sensitivity analysis of Level 4 of a 4 level AHP 
instance (Ord o,Set o,SetVal d,AHP3 a b c) => Sval o (Info4 o a b c d) c d where 
    sens' (v1,v2,v3,v4) = let v2'= mkOneTuple v2 `extendBy` v3 :: Val a (b,c)  
                          in sens (v1,projectInfo v2',v4)

-- -- =======================================================================






