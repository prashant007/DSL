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
policyInfo x = case x of Environment-> [Clinton --> 180,Trump --> 45] 
                         Economic -> [Clinton --> 55,Trump --> 220]
                         Foreign  -> [Clinton --> 35,Trump --> 140]  
                         Health   -> [Clinton --> 195,Trump --> 130]

policies :: Obj Candidate Policy
policies = gather policyInfo


-- - Every demography expressed what policies are important to them.
-- Young voters (300 in number) - 1/3 of youth voters plan to vote based on a candidates education policy, 1/6  each for economic and foregin policy,
-- and the remaining 1/3 for Health policy. 


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

candPriority :: Priority Candidate 
candPriority = priority candidates

expCandidate :: Explain (Policy,Demography,Geography)
expCandidate = explain vdCandidate

c11 = factorize trump :: Factor Population (Policy,Demography,Geography)
c12 = factorize clinton :: Factor Population (Policy,Demography,Geography)

vdCandidate :: Attr (Policy,Demography,Geography)
vdCandidate = reduce $ diff trump clinton

expDemography :: Explain Demography 
expDemography = generalize vdCandidate 

expGeography :: Explain Geography
expGeography = generalize vdCandidate 

expPolicy :: Explain Policy
expPolicy = generalize vdCandidate 


(_,support,barrier,mdss,_) = expCandidate

mdsC = head mdss :: Attr (Policy,Demography,Geography)


fact1 = pFact (factorize mdsC :: Factor Policy (Demography,Geography))
fact2 = pFact (factorize mdsC :: Factor Demography (Policy,Geography))
fact3 = pFact (factorize mdsC :: Factor Geography (Policy,Demography))

geographyTrump = pFact (factorize support :: Factor Geography (Policy,Demography))
geographyClinton = pFact (factorize barrier :: Factor Geography (Policy,Demography))

demographyTrump = pFact (factorize support :: Factor Demography (Policy,Geography))
demographyClinton = pFact (factorize barrier :: Factor Demography (Policy,Geography))

policyTrump = pFact (factorize support :: Factor Policy (Demography,Geography))
policyClinton = pFact (factorize barrier :: Factor Policy (Demography,Geography))

