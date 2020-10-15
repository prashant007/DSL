{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,KindSignatures,GADTs,DataKinds,TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass,FlexibleContexts,ScopedTypeVariables,UndecidableInstances #-}

import Record
import Info
import Valuation
import Data.Tuple.OneTuple (OneTuple(..))
import MDS
import Focus
import Transformation
import Stability



data Car     = Honda | BMW  deriving (Eq,Ord,Show,Enum,Bounded,Set)

data Feature = Price | MPG | Safety deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Opinion = Personal | Expert deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Weight  = Weighted deriving (Eq,Ord,Show,Enum,Bounded,Set)

instance Valence Feature where
  valence Price = False
  valence _     = True

instance Valence Opinion
instance Valence Weight


-- (1) Mapping carFeatures to opinions
--
carFeatures :: Info Car Feature
carFeatures = info [Honda --> [Price --> (34000 + 1179), MPG --> 30, Safety --> 9.8],
                    BMW   --> [Price --> 36000, MPG --> 32, Safety --> 9.1]]


-- (2) Creating a valuation from data (only for opinions)
--
vCarF :: Val Car Feature
vCarF = valuation carFeatures


-- (5) Adding dimensions and extending valuation
--
featureOpinions :: Info Feature Opinion
featureOpinions = info [Price  --> [Personal --> 5, Expert --> 3],
                        MPG    --> [Personal --> 3, Expert --> 5],
                        Safety --> [Personal --> 2, Expert --> 2]]


featureVal :: Val Car (OneTuple Feature)
featureVal = mkOneTuple vCarF

carOpinions :: Val Car (Feature,Opinion)
carOpinions = featureVal `extendBy` featureOpinions


{-
*Car> total carOpinions
{Honda -> 99.98, BMW -> 100.02}
*Car> total $ only Personal carOpinions
{Honda -> 50.37, BMW -> 49.63}
*Car> total $ only Expert carOpinions
{Honda -> 49.61, BMW -> 50.39}
-}


-- (6) Top-level: Weighting opinions
--
weight :: a -> [(Weight,a)]
weight x = [Weighted --> x]

weights :: Info Opinion Weight
weights = info [Personal --> weight 0.6,Expert --> weight 0.4]

cars :: Val Car (Feature,Opinion,Weight)
cars = carOpinions `extendBy` info [Personal --> weight (0.6),Expert --> weight 0.4]

carTuple = (carFeatures,featureOpinions,weights)
c11 = sens carTuple (Honda,BMW) :: Sens 1 Car (Car,Feature) 
c12 = sens carTuple (BMW,Honda) :: Sens 1 Car (Car,Feature) 
c2  = sens carTuple (Honda,BMW) :: Sens 2 Car (Feature,Opinion)
c3  = sens carTuple (Honda,BMW) :: Sens 3 Car (Opinion,Weight) 

t = total $ cars 



-- fv = valuation featureOpinions
-- wv = valuation weights

-- sensc3 = sens (vCarF1',fv,wv) (Honda,BMW) :: Sens 3 3 Car Opinion 
-- sensc2 = sens (vCarF1',fv,wv) (Honda,BMW) :: Sens 3 2 Car (Feature,Opinion)
-- sensc1 = sens (vCarF1',fv,wv) (Honda,BMW) :: Sens 3 1 Car Feature
-- -- total3 = total $ mkOneTuple vCarF `extendBy` fv `extendBy` wv

-- policyV :: Val Candidate Policy 
-- policyV = info [Trump   --> [Environment --> 0.2,Economic --> 0.8,Foreign --> 0.8,Health --> 0.4],
--                 Clinton --> [Environment --> 0.8,Economic --> 0.2,Foreign --> 0.2,Health --> 0.6]
--                 ]

-- demographyV :: Val Policy Demography
-- demographyV = info [Environment --> [Young --> 0.33,MiddleAged --> 0.21,Old --> 0.14],
--                     Economic    --> [Young --> 0.17,MiddleAged --> 0.37,Old --> 0.29],
--                     Foreign     --> [Young --> 0.17,MiddleAged --> 0.21,Old --> 0.14],
--                     Health      --> [Young --> 0.33,MiddleAged --> 0.21,Old --> 0.43]]


-- geographyV :: Val Demography Geography
-- geographyV = info [Young --> [Rural --> (0.2 - 0.0000),Urban --> 0.4],
--                    MiddleAged --> [Rural --> 0.3,Urban --> 0.4],
--                    Old --> [Rural --> 0.5,Urban --> 0.2]]

-- populationV :: Val Geography Population
-- populationV = addAttribute Population [Rural --> 0.5,Urban --> 0.5] objects

-- sensE1 = sens electTuple (Trump,Clinton) :: Sens 4 1 Candidate Policy 
-- sensE2 = sens electTuple (Trump,Clinton) :: Sens 4 2 Candidate (Policy,Demography)
-- sensE3 = sens electTuple (Trump,Clinton) :: Sens 4 3 Candidate (Demography,Geography)
-- sensE4 = sens electTuple (Trump,Clinton) :: Sens 4 4 Candidate Geography 


-- electTuple = (policyV,demographyV,geographyV,populationV)
-- totE = total $ mkOneTuple (valuation policyV) `extendBy` demographyV `extendBy` geographyV `extendBy` populationV



-- sensEx1 = pSensE $ allSens 1 (firstLevel,secondLevel)    
-- sensEx2 = pSensE $ allSens 2 (firstLevel,secondLevel)
--ptable = total $ mkOneTuple (valuation firstLevel) `extendBy` secondLevel


data Alt = A1 | A2 | A3 | A4 deriving (Eq,Ord,Show,Enum,Bounded,Set)

data C = C1 | C2 | C3 | C4 deriving (Eq,Ord,Show,Enum,Bounded,Set)

data W = W deriving (Eq,Ord,Show,Enum,Bounded,Set)


instance Valence Alt
instance Valence C 
instance Valence W 

firstLevel :: Val Alt C 
firstLevel = info [A1 --> [C1 --> 0.3088, C2 --> 0.2897, C3 --> 0.3867, C4 --> 0.1922],
                   A2 --> [C1 --> 0.2163, C2 --> 0.3458, C3 --> 0.1755, C4 --> 0.6288],
                   A3 --> [C1 --> 0.4509, C2 --> 0.2473, C3 --> 0.1194, C4 --> 0.0575],
                   A4 --> [C1 --> 0.0240, C2 --> 0.1172, C3 --> 0.3184, C4 --> 0.1215]] 

secondLevel :: Val C W 
secondLevel = info [C1 --> [W --> 0.3277],C2 --> [W --> 0.3058],
                    C3 --> [W --> 0.2876],C4 --> [W --> 0.0790]]



-- data Alt = A1 | A2 | A3 | A4 | A5 deriving (Eq,Ord,Show,Enum,Bounded,Set)

-- data C = C1 | C2 | C3 | C4 | C5 deriving (Eq,Ord,Show,Enum,Bounded,Set)

-- data W = W deriving (Eq,Ord,Show,Enum,Bounded,Set)


-- instance Valence Alt
-- instance Valence C 
-- instance Valence W 

-- firstLevel :: Val Alt C 
-- firstLevel = info [A1 --> [C1 --> 0.3576, C2 --> 0.2483, C3 --> 0.2899, C4 --> 0.2961, C5 --> 0.3202],
--                    A2 --> [C1 --> 0.3603, C2 --> 0.2836, C3 --> 0.0407, C4 --> 0.0939, C5 --> (0.0172)], 
--                    A3 --> [C1 --> 0.0255, C2 --> 0.1745, C3 --> 0.2895, C4 --> 0.2212, C5 --> 0.2641], 
--                    A4 --> [C1 --> 0.1609, C2 --> 0.2008, C3 --> 0.2960, C4 --> 0.0716, C5 --> 0.0315], 
--                    A5 --> [C1 --> 0.0957, C2 --> 0.0928, C3 --> 0.0839, C4 --> 0.3172, C5 --> 0.3670]] 


-- secondLevel :: Val C W 
-- secondLevel = info [C1 --> [W --> 0.4146],C2 --> [W --> 0.0129],C3 --> [W --> 0.2958],
--                     C4 --> [W --> 0.0604],C5 --> [W --> 0.2164]]
