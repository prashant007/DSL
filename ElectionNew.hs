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

elems :: [(Candidate,Policy,Demography,Geography)] 
elems = [(c,p,d,g) | c <- members,g <- members, d <- members, p <- members] 

totalInfo :: Attr (Candidate,Policy,Demography,Geography) 
totalInfo = f $ [600,400,410,620,140,100,200,300,550,475,160,175,
                 615,445,392,369,155,489,522,545,156,719,210,150] ++
                 [205,200,220,325,430,545,355,460,220,170,370,290,
                  140,220,385,120,780,390,370,265,414,645,747,889] 
    where
        f :: [Double] -> Attr (Candidate,Policy,Demography,Geography)
        f = mkAttr.zipWith (\x y -> x --> y) elems  


policies :: Obj Candidate Policy 
policies = crtObj totalInfo

policiesV :: Val Candidate Policy
policiesV = valuation policies

demography :: Obj Policy Demography
demography = crtObj totalInfo 

demographyV :: Val Policy Demography
demographyV = valuation demography

geography :: Obj Demography Geography
geography = crtObj totalInfo 

geographyV :: Val Demography Geography
geographyV = valuation geography


weights :: Obj Geography Weight
weights = addAttribute Weight (fromAttr x) objects
    where
      x = (sumOut totalInfo) :: Attr Geography

weightsV :: Val Geography Weight 
weightsV = valuation weights

candidates :: Val Candidate (Policy,Demography,Geography,Weight)
candidates = val policiesV `extendBy` demographyV `extendBy` geographyV `extendBy` weightsV 

type CandidateDecomp = Attr (Policy,Demography,Geography,Weight)


candPriority :: Priority Candidate 
candPriority = priority candidates

trump :: CandidateDecomp
trump = select Trump candidates

clinton :: CandidateDecomp
clinton = select Clinton candidates

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

expC1 :: Explain Demography 
expC1 = generalize vdCandidate 

expC2 :: Explain Geography
expC2 = generalize vdCandidate 

expC3 :: Explain Policy
expC3 = generalize vdCandidate 



