{-# LANGUAGE DeriveAnyClass,FlexibleContexts,ScopedTypeVariables#-}

import Record
import Info
import Valuation
import MDS
import Focus
import Transformation
import Stability
import Car 
import Election


fv = valuation featureOpinions
wv = valuation weights

sensc3 = sens33 (carsV,fv,wv) (Honda,BMW)
sensc2 = sens32 (carsV,fv,wv) (Honda,BMW)
sensc1 = sens31 (carsV,fv,wv) (Honda,BMW)
total3 = total $ mkOneTuple carsV `extendBy` fv `extendBy` wv



policyV :: Val Candidate Policy 
policyV = info [Trump   --> [Environment --> 0.2,Economic --> 0.8,Foreign --> 0.8,Health --> 0.4],
                Clinton --> [Environment --> 0.8,Economic --> 0.2,Foreign --> 0.2,Health --> 0.6]
                ]

demographyV :: Val Policy Demography
demographyV = info [Environment --> [Young --> 0.33,MiddleAged --> 0.21,Old --> 0.14],
                    Economic    --> [Young --> 0.17,MiddleAged --> 0.37,Old --> 0.29],
                    Foreign     --> [Young --> 0.17,MiddleAged --> 0.21,Old --> 0.14],
                    Health      --> [Young --> 0.33,MiddleAged --> 0.21,Old --> 0.43]]


geographyV :: Val Demography Geography
geographyV = info [Young --> [Rural --> (0.2 - 0.0000),Urban --> 0.4],
                   MiddleAged --> [Rural --> 0.3,Urban --> 0.4],
                   Old --> [Rural --> 0.5,Urban --> 0.2]]


populationV :: Info Geography Population
populationV = addAttribute Population [Rural --> 0.5,Urban --> 0.5] objects


sensE f = f electTuple (Trump,Clinton)

sensE1 = sensE sens41 
sensE2 = sensE sens42
sensE3 = sensE sens43
sensE4 = sensE sens44


electTuple = (policyV,demographyV,geographyV,populationV)
totE = total $ mkOneTuple (valuation policyV) `extendBy` demographyV `extendBy` geographyV `extendBy` populationV



sensEx1 = pSens $ allSens 1 (firstLevel,secondLevel)    
sensEx2 = pSens $ allSens 2 (firstLevel,secondLevel)
ptable = total $ mkOneTuple (valuation firstLevel) `extendBy` secondLevel


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
