{-# LANGUAGE FunctionalDependencies,FlexibleInstances,FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables,UndecidableInstances,ConstraintKinds #-}

module Explanation.SensitivityAnalysis where

import Encoding.Info
import Encoding.Valuation hiding (val)
import Encoding.Dimension

import Explanation.MDS
import Explanation.Transformation
import Explanation.SensDataType 
import Explanation.SensitivityHelper


class (Ord b,Set b,Set c,Covers a (Info b c),Valence c) =>
      Sensitivity o a b c | a o c -> b where
  sensitivityNorm :: a -> (o,o) -> c -> Change b 

sensitivity :: (Ord b,Norm a,Sensitivity o a b c,Limit  c) => 
               a -> (o,o) -> c -> Change b 
sensitivity a o c = mapChange (denormalize (project a) c) $ sensitivityNorm (norm a) o c 

-- ================================================================================
-- ========== Changeitivity Analysis For a 3 level AHP ==============================
type AHP2 o a b = (Ord o,Set o,SetVal2 a b) 

-- Sensitivity analysis of Level 1 of a 2 level AHP 
instance (Ord o,Set o,SetVal2 a b) => Sensitivity o (Val2 o a b) o a where
    sensitivityNorm v o a = mkChange . map (change21 v o a) $ members

-- Sensitivity analysis of Level 2 of a 2 level AHP 
instance (Ord o,Set o,SetVal2 a b) => Sensitivity o (Val2 o a b) a b where
    sensitivityNorm v o _ = mkChange . map (change22 v o) $ members 

-- -- ================================================================================
-- -- ========== Sensitivity Analysis For a 4 level AHP ==============================

-- Sensitivity analysis of Level 1 of a 3 level AHP 
instance (Set o,Ord2 o b,AHP3 a b c) => Sensitivity o (Val3 o a b c) o a where
    sensitivityNorm (v1,v2,v3) = let v'= mkOneTuple v2 `extendBy` v3 :: Val a (b,c)
                       in sensitivityNorm (v1,projectInfo v') 

-- Sensitivity analysis of Level 2 of a 3 level AHP 
instance (Ord2 o b,Valence3 a b c,AHP3 c a b) => Sensitivity o (Val3 o a b c) a b where
    sensitivityNorm v o b = let ls = [(x,b) | x <- members] 
                  in mkChange . map (change32 v o) $ ls 

-- Sensitivity analysis of Level 3 of a 3 level AHP 
instance (Set o,Ord2 o b,AHP3 c a b) => Sensitivity o (Val3 o a b c) b c where
    sensitivityNorm (v1,v2,v3)= let v'= mkOneTuple v1 `extendBy` v2 :: Val o (a,b)
                      in sensitivityNorm (projectInfo v',v3) 

-- =======================================================================

class FinVal a o b | a -> o b where
    finval :: a -> Val o b 

instance (Ord o,SetVal2 a b) => FinVal (Info2 o a b) o (a,b) where
    finval (x,y) = (mkOneTuple (valuation x) `extendBy` y)

instance (Ord o,SetVal3 a b c) => FinVal (Info3 o a b c) o (a,b,c) where
    finval (x,y,z) = (mkOneTuple (valuation x) `extendBy` y `extendBy` z)

instance (Ord o,SetVal4 a b c d) => FinVal (Info4 o a b c d) o (a,b,c,d) where
    finval (x,y,z,w) = (mkOneTuple (valuation x) `extendBy` y `extendBy` z `extendBy` w)


sensDefault :: (Ord o,Set2 b c,Sensitivity o a b c,Norm a, Limit  c) => 
               a -> Val o d -> c -> Change b 
sensDefault a v = sensitivity a (winner v,runnerUp v) 




