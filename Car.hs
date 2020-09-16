{-# LANGUAGE  DeriveAnyClass, LambdaCase #-}
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
data User    = Friend | Expert deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Car     = Honda | BMW | Toyota deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Feature = Price | MPG | Safety deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Weight  = Weight deriving (Eq,Ord,Show,Enum,Bounded,Set)

instance Valence Feature where
  valence Price = False
  valence _     = True

instance Valence User
instance Valence Weight


-- (1) Mapping cars to features
--

{-
-- Use this when exploring realistic values:
--
(c1N,c1P,c1M,c1S) = (Honda,34000,30,9.8)
(c2N,c2P,c2M,c2S) = (BMW,36000,33,9.1)

-- values :: Feature -> Spread Car
values :: Feature -> [(Car,Double)]
values Price  = [Honda --> 34000, BMW --> 36000]
values MPG    = [Honda --> 30,    BMW --> 33]
values Safety = [Honda --> 9.8,   BMW --> 9.1]

cars :: Info Car Feature
cars = gather values
-}

cars :: Info Car Feature
cars = info [Honda --> [Price --> 34000, MPG --> 30, Safety --> 9.8],
             BMW   --> [Price --> 36000, MPG --> 33, Safety --> 9.1]]

-- (2) creating a valuation from data (only for features)
--
carsV :: Val Car Feature
carsV = valuation cars

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
threeCars = addAlternative Toyota toyota cars


features3 = delAttribute cars Price

features4 = modAttribute cars Price Honda 45000


-- (3) Weighing attributes
--
fWeights :: Info Feature Weight
fWeights = addAttribute Weight [Price --> 1,MPG --> 1, Safety --> 1] objects
-- fWeights = addAttribute Weight [Price --> 1,MPG --> 2, Safety --> 1] objects

carsVW :: Val Car (Feature,Weight)
carsVW = mkOneTuple carsV `extendBy` fWeights


-- (4) Adding dimensions to car features (users and weights)
--
userInfo :: User -> Spread Feature
userInfo x = case x of Friend -> [Price --> 0.5, MPG  --> 0.3, Safety --> 0.2]
                       Expert -> [Price --> 0.2, MPG  --> 0.4, Safety --> 0.4]

users :: Info Feature User
users = gather userInfo

weights :: Info User Weight
weights = addAttribute Weight [Friend --> 0.6,Expert --> 0.4] objects



bRec :: Rec Feature
bRec = mkRec [Price --> 36000, MPG  --> 24, Safety --> 70]

bInfo :: Info Car Feature
bInfo = mkInfo [BMW --> bRec]

-- (5) Creating valuations for extended data
--
-- carsUF :: Val Car (User,Feature)
-- carsUF = extend cars users
--
-- carsWUF :: Val Car (Weight,User,Feature)
-- carsWUF = extend carsUF weights

cars' :: Val Car (Feature,User,Weight)
-- cars = val cars `extendBy` users `extendBy` weights
cars' = mkOneTuple carsV `extendBy` users `extendBy` weights

carPriority :: Priority Car
carPriority = priority cars'

-- ======================= FILTERING ELEMENTS FROM ANNOTATED VALUES ===========================
-- ============================================================================================

-- (6) Explaining decisions
--
type CarDecomp = Rec (Feature,User,Weight)

-- Valuations for specific cars
--
-- honda :: CarDecomp
-- honda = select Honda cars
--
-- bmw :: CarDecomp
-- bmw = select BMW cars
--
vdCar :: CarDecomp
vdCar = select Honda cars' `diff` select BMW cars'

exp0 :: Explain (Feature,User,Weight)
exp0 = explain vdCar


explCar :: Explain (Feature,User,Weight)
explCar = explain vdCar


showMDSexp0 = pmds exp0
showDomexp0 = pdom exp0

exp1 :: Explain User
exp1 = generalize vdCar

exp2 :: Explain Feature
exp2 = generalize vdCar

mds0 :: MDS (Feature,User)
mds0 = let (_,_,_,ms,_) = exp0
       in  reduce (head ms)::Rec (Feature,User)

m01 = pFact (factorize mds0 :: Factor Feature User)
m02 = pFact (factorize mds0 :: Factor User Feature)
-- m11 = factorize mds1 :: Factor Weight (User,Feature)
