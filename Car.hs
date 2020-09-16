{-# LANGUAGE DeriveAnyClass #-}
module Car where

import qualified Data.Map.Strict as M
import Data.Tuple.OneTuple (only,OneTuple(..))
import Prelude hiding (compare)

import Record
import Info
import Valuation
import MDS
import Transformation


-- Car Example
--
data Car     = Honda | BMW | Toyota deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Feature = Price | MPG | Safety deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Opinion = Friend | Expert deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Weight  = Weight deriving (Eq,Ord,Show,Enum,Bounded,Set)

instance Valence Feature where
  valence Price = False
  valence _     = True

instance Valence Opinion
instance Valence Weight


-- (1) Mapping carFeatures to opinions
--
carFeatures :: Info Car Feature
carFeatures = info [Honda --> [Price --> 34000, MPG --> 30, Safety --> 9.8],
                    BMW   --> [Price --> 36000, MPG --> 33, Safety --> 9.1]]


-- (2) creating a valuation from data (only for opinions)
--
carsV :: Val Car Feature
carsV = valuation carFeatures

compare :: (Eq o,Ord r) => Val o r -> o -> o -> Rec r
compare i o1 o2 = i!o1 - i!o2

hvb :: Ord r => Val Car r -> Rec r
hvb i = compare i Honda BMW

-- (3) Some variation: adding/deleting/modifying a feature attribute
--
toyota :: Feature -> Double
toyota Price  = 27000
toyota Safety = 9.4
toyota MPG    = 30


threeCars :: Info Car Feature
threeCars = addAlternative Toyota toyota carFeatures

opinions3 = delAttribute carFeatures Price
opinions4 = modAttribute carFeatures Price Honda 45000


-- (3) Weighing attributes
--
fWeights :: Info Feature Weight
fWeights = addAttribute Weight [Price --> 1,MPG --> 1, Safety --> 1] objects
-- fWeights = addAttribute Weight [Price --> 1,MPG --> 2, Safety --> 1] objects

carsVW :: Val Car (Feature,Weight)
carsVW = mkOneTuple carsV `extendBy` fWeights


-- (4) Adding dimensions to car opinions (opinions and weights)
--
featureOpinions :: Info Feature Opinion
featureOpinions = info [Price  --> [Friend --> 0.5, Expert --> 0.2],
                        MPG    --> [Friend --> 0.3, Expert --> 0.4],
                        Safety --> [Friend --> 0.2, Expert --> 0.4]]

weights :: Info Opinion Weight
weights = addAttribute Weight [Friend --> 0.6,Expert --> 0.4] objects


bRec :: Rec Feature
bRec = mkRec [Price --> 36000, MPG  --> 24, Safety --> 70]

bInfo :: Info Car Feature
bInfo = mkInfo [BMW --> bRec]


-- (5) Creating valuations for extended data
--
-- carsUF :: Val Car (Opinion,Feature)
-- carsUF = extend carFeatures featureOpinions
--
-- carsWUF :: Val Car (Weight,Opinion,Feature)
-- carsWUF = extend carsUF weights

cars :: Val Car (Feature,Opinion,Weight)
-- carFeatures = val carFeatures `extendBy` featureOpinions `extendBy` weights
cars = mkOneTuple carsV `extendBy` featureOpinions `extendBy` weights

carPriority :: Priority Car
carPriority = priority cars


-- (6) Explaining decisions
--
type CarDecomp = Norm (Feature,Opinion,Weight)

-- Valuations for specific cars
--
-- honda :: CarDecomp
-- honda = select Honda cars
--
-- bmw :: CarDecomp
-- bmw = select BMW cars
--
vdCar :: CarDecomp
vdCar = select Honda cars `diff` select BMW cars

exp0 :: Explain (Feature,Opinion,Weight)
exp0 = explain vdCar


explCar :: Explain (Feature,Opinion,Weight)
explCar = explain vdCar


showMDSexp0 = pmds exp0
showDomexp0 = pdom exp0

exp1 :: Explain Opinion
exp1 = generalize vdCar

exp2 :: Explain Feature
exp2 = generalize vdCar

mds0 :: MDS (Feature,Opinion)
mds0 = let (_,_,_,ms,_) = exp0
       in  reduce (head ms)::Norm (Feature,Opinion)

m01 = pFact (factorize mds0 :: Factor Feature Opinion)
m02 = pFact (factorize mds0 :: Factor Opinion Feature)
-- m11 = factorize mds1 :: Factor Weight (Opinion,Feature)
