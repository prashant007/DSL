{-# LANGUAGE  DeriveAnyClass #-}
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


instance AttrValence Geography
instance AttrValence Demography
instance AttrValence Policy

-- Alternative: doing it in one step
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
demographyV =  valuation demography

geographyInfo :: Geography -> Spread Demography 
geographyInfo x = case x of Rural -> [Young --> 30,MiddleAged --> 30,Old --> 40]
                            Urban -> [Young --> 30,MiddleAged --> 40,Old --> 30]

geography :: Obj Demography Geography
geography = gather geographyInfo

geographyV :: Val Demography Geography
geographyV = valuation geography

weightsE :: Val Geography Weight
weightsE = addAttribute Weight [Rural --> 0.6,Urban --> 0.4] objects

candidateVal :: Val Candidate (Weight,Geography,Demography,Policy)
candidateVal = policiesV `extend` demographyV `extend` geographyV `extend` weightsE

type CandidateDecomp = Attr (Weight,Geography,Demography,Policy)

trump :: CandidateDecomp
trump = select Trump candidateVal

clinton :: CandidateDecomp
clinton = select Clinton candidateVal

c11 = factorize trump :: Factor Weight (Geography,Demography,Policy)
c12 = factorize clinton :: Factor Weight (Geography,Demography,Policy)

vdCandidate :: Attr (Geography,Demography,Policy)
vdCandidate = denoise $ diff trump clinton

(c2a,c2b,c2c,c2d,c2e) = explain vdCandidate :: Explain (Geography,Demography,Policy)

c21 :: Factor Geography (Demography,Policy)
c21 = factorize c2c 

c22 :: Factor Demography (Geography,Policy)
c22 = factorize c2c 

c23 :: Factor Policy (Geography,Demography)
c23 = factorize c2c 

-- c24 :: Factor (Policy,Geography) Demography
-- c24 = factorize c2c 

expC1 :: Explain Demography 
expC1 = generalize vdCandidate