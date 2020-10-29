{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,DataKinds#-}
{-# LANGUAGE FlexibleContexts,ScopedTypeVariables,UndecidableInstances,ConstraintKinds,TypeApplications #-}

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


-- Helper functions are in StabilityHelper
class (Ord b,Set2 b c,Covers a (Info b c),Valence c,Valtuple a) => Sval o a b c | a -> o b c where
    sens' :: a -> (o,o) -> c -> Sens b

-- maybe add percentages here
sens  :: (Ord b,Sval o a b c,Bound c) => a -> (o,o) -> c -> Sens b
sens a o c =  mapSens (denormalize (project a) c) $ sens' (valtuple a) o c 

-- ================================================================================
-- ========== Sensitivity Analysis For a 2 level AHP ==============================
type AHP2 o a b = (Ord o,Set o,SetVal2 a b) 

-- Sensitivity analysis of Level 1 of a 2 level AHP 
instance (Ord o,Set o,SetVal2 a b) => Sval o (Val2 o a b) o a where
    sens' v o a = mkSens . map (change21 v o a) $ members

-- Sensitivity analysis of Level 2 of a 2 level AHP 
instance (Ord o,Set o,SetVal2 a b) => Sval o (Val2 o a b) a b where
    sens' v o _ = mkSens . map (change22 v o) $ members 

-- -- ================================================================================
-- -- ========== Sensitivity Analysis For a 3 level AHP ==============================

-- Sensitivity analysis of Level 1 of a 3 level AHP 
instance (Set o,Ord2 o b,AHP3 a b c) => Sval o (Val3 o a b c) o a where
    sens' (v1,v2,v3) = let v'= mkOneTuple v2 `extendBy` v3 :: Val a (b,c)
                       in sens' (v1,projectInfo v') 

-- Sensitivity analysis of Level 2 of a 3 level AHP 
instance (Ord2 o b,Valence3 a b c,AHP3 c a b) => Sval o (Val3 o a b c) a b where
    sens' v o b = let ls = [(x,b) | x <- members] 
                  in mkSens . map (change32 v o) $ ls 

-- Sensitivity analysis of Level 3 of a 3 level AHP 
instance (Set o,Ord2 o b,AHP3 c a b) => Sval o (Val3 o a b c) b c where
    sens' (v1,v2,v3)= let v'= mkOneTuple v1 `extendBy` v2 :: Val o (a,b)
                      in sens' (projectInfo v',v3) 


-- =======================================================================

class FinVal a o b | a -> o b where
    finval :: a -> Val o b 

instance (Ord o,SetVal2 a b) => FinVal (Info2 o a b) o (a,b) where
    finval (x,y) = (mkOneTuple (valuation x) `extendBy` y)

instance (Ord o,SetVal3 a b c) => FinVal (Info3 o a b c) o (a,b,c) where
    finval (x,y,z) = (mkOneTuple (valuation x) `extendBy` y `extendBy` z)

instance (Ord o,SetVal4 a b c d) => FinVal (Info4 o a b c d) o (a,b,c,d) where
    finval (x,y,z,w) = (mkOneTuple (valuation x) `extendBy` y `extendBy` z `extendBy` w)


sensTopTwo' :: (Ord o,Sval o a b c, Bound c) => a -> Val o d -> c -> Sens b 
sensTopTwo' a v = sens a (winner v,runnerUp v) 

-- class (Ord o,Sval o a b c, FinVal a o d,Bound c) => SvalDefault o a b c d | a -> o b c d where
--     sensTopTwo :: a -> Val o d -> c -> Sens b 
--     sensTopTwo a _ = let v = finval a in sens a (winner v,runnerUp v) 

class (Ord o,Sval o a b c, FinVal a o d,Bound c) => SvalDefault o a b c d | a -> o b c d where
    sensTopTwo :: a -> c -> Sens b 
    sensTopTwo a = let v = finval a in sens a (winner v,runnerUp v) 


instance (Set o,SetVal2 a b,Bound a) => SvalDefault o (Val2 o a b) o a (a,b) where {}
instance (Set o,SetVal2 a b,Bound b) => SvalDefault o (Val2 o a b) a b (a,b) where {}

instance (Set o,AHP3 a b c,Bound a) => SvalDefault o (Val3 o a b c) o a (a,b,c) where {}
instance (Set o,AHP3 c a b,Bound b) => SvalDefault o (Val3 o a b c) a b (a,b,c) where {}
instance (Set o,AHP3 a b c,Bound c) => SvalDefault o (Val3 o a b c) b c (a,b,c) where {}


