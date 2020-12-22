{-# LANGUAGE  DeriveAnyClass #-}
module Election where

import Record
import Info
import Factor  
import Valuation
import MDS
import Dimension
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

-- demography :: Info Policy Demography
-- demography = info [Environment --> [Young --> 100,MiddleAged --> 75, Old --> 50],
--                    Economic    --> [Young --> 50, MiddleAged --> 125,Old --> 100],
--                    Foreign     --> [Young --> 50, MiddleAged --> 75, Old --> 50],
--                    Health      --> [Young --> 100,MiddleAged --> 75, Old --> 150]]


demography :: Info Policy Demography
demography = info [Environment --> [Young --> 100,MiddleAged --> 75, Old --> 50],
                   Economic    --> [Young --> 25, MiddleAged --> 125,Old --> 100],
                   Foreign     --> [Young --> 75, MiddleAged --> 125, Old --> 50],
                   Health      --> [Young --> 100,MiddleAged --> 25, Old --> 150]]

  
geography :: Info Demography Geography
geography = info [Young --> [Rural --> 100,Urban --> 200],
                  MiddleAged --> [Rural --> 150,Urban --> 200],
                  Old --> [Rural --> 250,Urban --> 100]]

  



population :: Info Geography Population
population = addAttribute Population [Rural --> 500,Urban --> 500] objects

candidates :: Val Candidate (Policy,Demography,Geography,Population)
candidates = val policy `extendBy` demography `extendBy` geography `extendBy` population

candidates' :: Val Candidate (Policy,Demography,Geography)
candidates' = shrinkVal candidates


vdE :: Rec (Policy,Demography,Geography)
vdE = diff (shrinkVal candidates) Trump Clinton


type CandidateDecomp = Rec (Policy,Demography,Geography,Population)

trump :: CandidateDecomp
trump = select Trump candidates

clinton :: CandidateDecomp
clinton = select Clinton candidates



expCandidate :: Explanation Candidate (Policy,Demography,Geography)
expCandidate = explain candidates'

-- c11 = factorize trump :: Focus Population (Policy,Demography,Geography)
-- c12 = factorize clinton :: Focus Population (Policy,Demography,Geography)

vdCandidate :: Rec (Policy,Demography,Geography)
vdCandidate = candidates'!Trump - candidates'!Clinton

vdDemo :: Rec Demography 
vdDemo = focus vdCandidate  

vdGeo :: Rec Geography 
vdGeo = focus vdCandidate 

vdPol :: Rec Policy 
vdPol = focus vdCandidate 


expDemography :: Explanation Candidate Demography 
expDemography = explain $ focus candidates'  

expGeography :: Explanation Candidate Geography
expGeography = explain $ focus candidates'  

expPolicy :: Explanation Candidate Policy
expPolicy = explain $ focus candidates'  


-- (_,support,barrier,mdss,_) = expCandidate

-- mdsC = head mdss :: Rec (Policy,Demography,Geography)


-- fact1 = factorize mdsC :: Focus Policy (Demography,Geography)
-- fact2 = factorize mdsC :: Focus Demography (Policy,Geography)
-- fact3 = factorize mdsC :: Focus Geography (Policy,Demography)

-- geographyTrump = factorize support :: Focus Geography (Policy,Demography)
-- geographyClinton = factorize barrier :: Focus Geography (Policy,Demography)

-- demographyTrump = factorize support :: Focus Demography (Policy,Geography)
-- demographyClinton = factorize barrier :: Focus Demography (Policy,Geography)

-- policyTrump = factorize support :: Focus Policy (Demography,Geography)
-- policyClinton = factorize barrier :: Focus Policy (Demography,Geography)

