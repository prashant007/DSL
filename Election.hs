{-# LANGUAGE  DeriveAnyClass,MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}
module Election where

import Attribute
import Object
import Valuation
import MDS
import Transformation

import qualified Data.Map as M

-- =============================================================================
-- ====================== ELECTION EXAMPLE =====================================

data Geography = Rural | Urban deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Demography = Young | MiddleAged | Old deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Policy = Education | Economic | Foreign | Health deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Candidate = Clinton | Trump deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Weight  = Weight deriving (Eq,Ord,Show,Enum,Bounded,Set)

instance AttrValence Geography
instance AttrValence Demography
instance AttrValence Policy
instance AttrValence Weight

-- Doing it in one step
--
policyInfo :: Policy -> Spread Candidate
policyInfo x = case x of Education-> [Clinton --> 30,Trump --> 70]
                         Economic -> [Clinton --> 60,Trump --> 40]
                         Foreign  -> [Clinton --> 70,Trump --> 30] 
                         Health   -> [Clinton --> 20,Trump --> 80 ]

policies :: Obj Candidate Policy
policies = gather policyInfo

policiesV :: Val Candidate Policy
policiesV = valuation policies

demographyInfo :: Demography -> Spread Policy
demographyInfo x = case x of Young -> [Education --> 20 ,Economic --> 30 ,Foreign --> 20,Health --> 30]
                             MiddleAged -> [Education --> 15,Economic --> 35,Foreign --> 35,Health --> 15]
                             Old -> [Education --> 10,Economic --> 50,Foreign --> 20,Health --> 20]

demography :: Obj Policy Demography
demography = gather demographyInfo

demographyV :: Val Policy Demography
demographyV = valuation demography

geographyInfo :: Geography -> Spread Demography 
geographyInfo x = case x of Rural -> [Young --> 30,MiddleAged --> 30,Old --> 40]
                            Urban -> [Young --> 30,MiddleAged --> 40,Old --> 30]

geography :: Obj Demography Geography
geography = gather geographyInfo

geographyV :: Val Demography Geography
geographyV = valuation geography

weightsE :: Val Geography Weight
weightsE = addAttribute Weight [Rural --> 0.6,Urban --> 0.4] objects

candidateVal :: Val Candidate (Policy,Demography,Geography,Weight)
candidateVal = val policiesV `extendBy` demographyV `extendBy` geographyV `extendBy` weightsE

type CandidateDecomp = Attr (Policy,Demography,Geography,Weight)

trump :: CandidateDecomp
trump = select Trump candidateVal

clinton :: CandidateDecomp
clinton = select Clinton candidateVal

c11 = factorize trump :: Factor Weight (Policy,Demography,Geography)
c12 = factorize clinton :: Factor Weight (Policy,Demography,Geography)

vdCandidate :: Attr (Policy,Demography,Geography)
vdCandidate = denoise $ diff trump clinton

(c2a,c2b,c2c,c2d,c2e) = explain vdCandidate :: Explain (Policy,Demography,Geography)

c21 :: Factor Geography (Policy,Demography)
c21 = factorize c2c 

c22 :: Factor Demography (Policy,Geography)
c22 = factorize c2c 

c23 :: Factor Policy (Demography,Geography)
c23 = factorize c2c 

-- c24 :: Factor (Policy,Geography) Demography
-- c24 = factorize c2c 

expC1 :: Explain Demography 
expC1 = generalize vdCandidate 


-- [(Rural,Young,Education),(Rural,Young,Economic),(Rural,Young,Foreign),(Rural,Young,Health),
-- (Rural,MiddleAged,Education),(Rural,MiddleAged,Economic),(Rural,MiddleAged,Foreign),
-- (Rural,MiddleAged,Health),(Rural,Old,Education),(Rural,Old,Economic),
-- (Rural,Old,Foreign),(Rural,Old,Health),(Urban,Young,Education),(Urban,Young,Economic),(Urban,Young,Foreign),
-- (Urban,Young,Health),(Urban,MiddleAged,Education),(Urban,MiddleAged,Economic),(Urban,MiddleAged,Foreign),(Urban,MiddleAged,Health),
-- (Urban,Old,Education),(Urban,Old,Economic),(Urban,Old,Foreign),(Urban,Old,Health)]


elems :: [(Geography,Demography,Policy) ] 
elems = [(g,d,p) | g <- members, d <- members, p <- members]


totalInfo :: Candidate -> Attr (Geography,Demography,Policy)
totalInfo x = case x of Trump   -> f [100,200,110,120,140,130,200,210,220,125,220,175,
                                     115,145,132,269,155,489,122,45,56,89,110,150]
                        Clinton -> f [225,220,100,225,130,245,155,160,180,270,170,90,
                                     40,220,115,120,80,90,70,65,114,145,147,189] 
    where
        elems = [(g,d,p) | g <- members, d <- members, p <- members] 
        f :: [Double] -> Attr (Geography,Demography,Policy)
        f = mkAttr.zipWith (\x y -> x --> y) elems  


policyInfo :: Obj Candidate Policy 
policyInfo = undefined 

demographyInfo :: Obj Policy Demography
demographyInfo = undefined 

geographyInfo :: Obj Demography Geography
geographyInfo = undefined 




 -- Obj Candidate Policy 
 -- Obj Policy Demography
 -- Obj Demography Geography
 -- Obj Geography Weight            


-- {Trump ->
-- {Education -> ,
--  Economic -> 0.200,
--  Foregin ->,
--  Health -> },
--  Fuel ->
-- {Clinton ->  
-- {Education -> ,
--  Economic -> 0.200,
--  Foregin ->,
--  Health -> }}



-- featureInfo :: Feature -> Spread Car
-- featureInfo x = case x of Price  -> [Honda --> 24000,BMW --> 36000]
--                           Safety -> [Honda --> 30,BMW --> 70]
--                           Fuel   -> [Honda --> 36,BMW --> 24]

-- features :: Obj Car Feature
-- features = gather featureInfo


-- userInfo :: User -> Spread Feature
-- userInfo x = case x of Friend -> [Price --> 0.5, Fuel --> 0.3, Safety --> 0.2]
--                        Expert -> [Price --> 0.2, Fuel --> 0.4, Safety --> 0.4]

-- users :: Obj Feature User
-- users = gather userInfo

-- weights :: Obj User Weight
-- weights = addAttribute Weight [Friend --> 0.6,Expert --> 0.4] objects

-- users :: Obj Candidate (Geography,Demography,Policy)
-- users = gather userInfo
