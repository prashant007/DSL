{-# LANGUAGE DeriveAnyClass #-}
module Car where

import qualified Data.Map.Strict as M
import Data.Tuple.OneTuple (OneTuple(..))

import Record
import Info
import Valuation
import MDS
import Transformation


-- Car Example
--
data Car     = Honda | BMW | Toyota deriving (Eq,Ord,Show,Enum,Bounded,Set)
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
carFeatures = info [Honda --> [Price --> 34000, MPG --> 30, Safety --> 9.8],
                    BMW   --> [Price --> 36000, MPG --> 33, Safety --> 9.1]]


-- (2) creating a valuation from data (only for opinions)
--
carsV :: Val Car Feature
carsV = valuation carFeatures


-- (3) Some variation: adding/deleting/modifying a feature attribute
--
threeCars :: Info Car Feature
threeCars = carFeatures `union`
            info [Toyota --> [Price --> 27000, MPG --> 30, Safety --> 9.4]]

opinions3 = delAttribute carFeatures Price
opinions4 = modAttribute carFeatures Price Honda 45000

vdThreeCars :: Rec Feature
vdThreeCars = diff (valuation threeCars) Honda BMW

-- aiThreeCars :: Factor Feature ()
-- aiThreeCars = factorize (mkOneTuple vdThreeCars)

-- (3) Weighing attributes
--
weight :: a -> [(Weight,a)]
weight x = [Weighted --> x]

equalWeights :: Info Feature Weight
equalWeights = info [Price  --> weight 1,
                     MPG    --> weight 1,
                     Safety --> weight 1]

personalWeights :: Info Feature Weight
personalWeights = info [Price  --> weight 5,
                        MPG    --> weight 3,
                        Safety --> weight 2]
-- equalWeights = addAttribute Weighted [Price --> 1,MPG --> 1, Safety --> 1] objects
-- equalWeights = addAttribute Weighted [Price --> 1,MPG --> 2, Safety --> 1] objects

carEW :: Val Car (Feature,Weight)
carEW = mkOneTuple carsV `extendBy` equalWeights

carPW :: Val Car (Feature,Weight)
carPW = mkOneTuple carsV `extendBy` personalWeights


-- (4) Adding dimensions to car opinions (opinions and weights)
--
featureOpinions :: Info Feature Opinion
-- featureOpinions = info [Price  --> [Personal --> 0.5, Expert --> 0.2],
--                         MPG    --> [Personal --> 0.3, Expert --> 0.4],
--                         Safety --> [Personal --> 0.2, Expert --> 0.4]]
featureOpinions = info [Price  --> [Personal --> 5, Expert --> 3],
                        MPG    --> [Personal --> 3, Expert --> 5],
                        Safety --> [Personal --> 2, Expert --> 2]]

weights :: Info Opinion Weight
-- weights = addAttribute Weighted [Personal --> 0.6,Expert --> 0.4] objects
weights = info [Personal --> weight 0.6,Expert --> weight 0.4]

onlyPersonal :: Info Opinion Weight
onlyPersonal = addAttribute Weighted [Personal --> 1,Expert --> 0] objects

bRec :: Rec Feature
bRec = mkRec [Price --> 36000, MPG  --> 24, Safety --> 70]

bInfo :: Info Car Feature
bInfo = mkInfo [BMW --> bRec]


-- (5) Creating valuations for extended data
--
featureVal :: Val Car (OneTuple Feature)
featureVal = mkOneTuple carsV

carOpinions :: Val Car (Feature,Opinion)
carOpinions = featureVal `extendBy` featureOpinions
--
-- carsWUF :: Val Car (Weight,Opinion,Feature)
-- carsWUF = extend carsUF weights

cars :: Val Car (Feature,Opinion,Weight)
-- carFeatures = val carFeatures `extendBy` featureOpinions `extendBy` weights
-- cars = mkOneTuple carsV `extendBy` featureOpinions `extendBy` weights
-- cars = carOpinions `extendBy` weights
cars = carOpinions `extendBy` info [Personal --> weight 0.6,Expert --> weight 0.4]

carsP :: Val Car (Feature,Opinion,Weight)
-- carFeatures = val carFeatures `extendBy` featureOpinions `extendBy` weights
carsP = mkOneTuple carsV `extendBy` featureOpinions `extendBy` onlyPersonal


-- carPriority :: Priority Car
-- carPriority = priority cars


-- (6) Explaining decisions
--
type CarDecomp = Rec (Feature,Opinion,Weight)

-- Valuations for specific cars
--
-- honda :: CarDecomp
-- honda = select Honda cars
--
-- bmw :: CarDecomp
-- bmw = select BMW cars
--
vdCar :: CarDecomp
vdCar = cars!Honda - cars!BMW

exp0 :: Explain (Feature,Opinion,Weight)
exp0@(vd0,sup0,bar0,doms0,mds0:_) = explain vdCar

exp1 :: Explain Opinion
exp1 = generalize vdCar

exp2 :: Explain Feature
exp2 = generalize vdCar

mds0r :: MDS (Feature,Opinion)
mds0r = reduce mds0

featureFocus = pFact (factorize mds0r :: Factor Feature Opinion)
opinionFocus = pFact (factorize mds0r :: Factor Opinion Feature)


factorize' = factorize.mkOneTupleRec

vdTwoCars = diff (valuation carFeatures) Honda BMW
factTwoCars = factorize' vdTwoCars :: Factor Feature ()
factThreeCars = factorize' vdThreeCars :: Factor Feature ()
