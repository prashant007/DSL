{-# LANGUAGE  DeriveAnyClass,MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}
module Election where

import Record
import Info
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

instance Valence Geography
instance Valence Demography
instance Valence Policy
instance Valence Population


policy :: Info Candidate Policy 
policy = info [Clinton --> [Environment --> 180,Economic --> 55, Foreign --> 35, Health --> 195],
               Trump   --> [Environment --> 45, Economic --> 220,Foreign --> 140,Health --> 130]]



-- - Every demography expressed what policies are important to them.
-- Young voters (300 in number) - 1/3 of youth voters plan to vote based on a candidates education policy, 1/6  each for economic and foregin policy,
-- and the remaining 1/3 for Health policy. 

demography :: Info Policy Demography
demography = info [Environment --> [Young --> 100,MiddleAged --> 75, Old --> 50],
                   Economic    --> [Young --> 50, MiddleAged --> 125,Old --> 100],
                   Foreign     --> [Young --> 50, MiddleAged --> 75, Old --> 50],
                   Health      --> [Young --> 100,MiddleAged --> 75, Old --> 150]]

  

geography :: Info Demography Geography
geography = info [Young --> [Rural --> 100,Urban --> 200],
                  MiddleAged --> [Rural --> 150,Urban --> 200],
                  Old --> [Rural --> 250,Urban --> 100]]


population :: Info Geography Population
population = addAttribute Population [Rural --> 500,Urban --> 500] objects

candidates :: Val Candidate (Policy,Demography,Geography,Population)
candidates = val policy `extendBy` demography `extendBy` geography `extendBy` population

type CandidateDecomp = Rec (Policy,Demography,Geography,Population)

trump :: CandidateDecomp
trump = select Trump candidates

clinton :: CandidateDecomp
clinton = select Clinton candidates

-- candPriority :: Priority Candidate 
-- candPriority = priority candidates

expCandidate :: Explain (Policy,Demography,Geography)
expCandidate = explain vdCandidate

c11 = factorize trump :: Factor Population (Policy,Demography,Geography)
c12 = factorize clinton :: Factor Population (Policy,Demography,Geography)

vdCandidate :: Rec (Policy,Demography,Geography)
vdCandidate = reduce $ candidates!Trump - candidates!Clinton

expDemography :: Explain Demography 
expDemography = generalize vdCandidate 

expGeography :: Explain Geography
expGeography = generalize vdCandidate 

expPolicy :: Explain Policy
expPolicy = generalize vdCandidate 


(_,support,barrier,mdss,_) = expCandidate

mdsC = head mdss :: Rec (Policy,Demography,Geography)


fact1 = pFact (factorize mdsC :: Factor Policy (Demography,Geography))
fact2 = pFact (factorize mdsC :: Factor Demography (Policy,Geography))
fact3 = pFact (factorize mdsC :: Factor Geography (Policy,Demography))

geographyTrump = pFact (factorize support :: Factor Geography (Policy,Demography))
geographyClinton = pFact (factorize barrier :: Factor Geography (Policy,Demography))

demographyTrump = pFact (factorize support :: Factor Demography (Policy,Geography))
demographyClinton = pFact (factorize barrier :: Factor Demography (Policy,Geography))

policyTrump = pFact (factorize support :: Factor Policy (Demography,Geography))
policyClinton = pFact (factorize barrier :: Factor Policy (Demography,Geography))

