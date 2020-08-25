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
data Policy = Environment | Economic | Foreign | Health deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Candidate = Clinton | Trump deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Population  = Population deriving (Eq,Ord,Show,Enum,Bounded,Set)

instance AttrValence Geography
instance AttrValence Demography
instance AttrValence Policy
instance AttrValence Population

-- Doing it in one step
--
policyInfo :: Policy -> Spread Candidate
policyInfo x = case x of Environment-> [Clinton --> 135,Trump --> 55] 
                         Economic -> [Clinton --> 90,Trump --> 220]
                         Foreign  -> [Clinton --> 90,Trump --> 220]  
                         Health   -> [Clinton --> 135,Trump --> 55]

policies :: Obj Candidate Policy
policies = gather policyInfo

demographyInfo :: Demography -> Spread Policy
demographyInfo x = case x of Young -> [Environment --> 100,Economic --> 50,Foreign --> 50,Health --> 100]
                             MiddleAged -> [Environment --> 75,Economic --> 125,Foreign --> 75,Health --> 75]
                             Old -> [Environment --> 50,Economic --> 100,Foreign --> 50,Health --> 150]

demography :: Obj Policy Demography
demography = gather demographyInfo
    
geographyInfo :: Geography -> Spread Demography 
geographyInfo x = case x of Rural -> [Young --> 100,MiddleAged --> 150,Old --> 250]
                            Urban -> [Young --> 200,MiddleAged --> 200,Old --> 100]

geography :: Obj Demography Geography
geography = gather geographyInfo

population :: Obj Geography Population
population = addAttribute Population [Rural --> 500,Urban --> 500] objects

populationInfo :: Population -> Spread Geography 
populationInfo Population = [Rural --> 500,Urban --> 500]

candidates :: Val Candidate (Policy,Demography,Geography,Population)
candidates = val policies `extendBy` demography `extendBy` geography `extendBy` population


candidates' :: Val Candidate (Policy,Demography,Geography,Population)
candidates' = val' policyInfo `extend` demographyInfo `extend` geographyInfo `extend` populationInfo

type CandidateDecomp = Attr (Policy,Demography,Geography,Population)

trump :: CandidateDecomp
trump = select Trump candidates

clinton :: CandidateDecomp
clinton = select Clinton candidates

c11 = factorize trump :: Factor Population (Policy,Demography,Geography)
c12 = factorize clinton :: Factor Population (Policy,Demography,Geography)

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

expC2 :: Explain Geography
expC2 = generalize vdCandidate 

expC3 :: Explain Policy
expC3 = generalize vdCandidate 



