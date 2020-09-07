{-# LANGUAGE  DeriveAnyClass, LambdaCase #-}
module Car where

import qualified Data.Map as M
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

instance AttrValence Feature where
   valence Price  = Neg
   valence MPG    = Pos
   valence Safety = Pos

instance AttrValence User
instance AttrValence Weight


-- (1) Mapping cars to features
--

{-
-- Use this when exploring realistic values:
--
(c1N,c1P,c1M,c1S) = (Honda,34000,30,9.8)
(c2N,c2P,c2M,c2S) = (BMW,36000,33,9.1)

featuresP :: Info Car Feature
featuresP = addAttribute Price [Honda --> c1P,BMW --> c2P] objects

featuresS :: Info Car Feature
featuresS = addAttribute Safety [Honda --> c1S,BMW --> c2S] featuresP

featuresF :: Info Car Feature
featuresF = addAttribute MPG  [Honda --> c1M,BMW --> c2M] featuresS

-- Alternative: doing it in one step
--
features :: Feature -> Spread Car
features = \case Price  -> [Honda --> c1P,BMW --> c2P]
                 Safety -> [Honda --> c1S,BMW --> c2S]
                 MPG    -> [Honda --> c1M,BMW --> c2M]
-}

-- values :: Feature -> Spread Car
values :: Feature -> [(Car,Double)]
values Price  = [Honda --> 34000, BMW --> 36000]
values MPG    = [Honda --> 30,    BMW --> 33]
values Safety = [Honda --> 9.8,   BMW --> 9.1]

carsF :: Info Car Feature
-- carsF = gather values
carsF = info [Honda --> [Price --> 34000, MPG --> 30, Safety --> 9.8],
              BMW   --> [Price --> 36000, MPG --> 33, Safety --> 9.1]]

-- (2) creating a valuation from data (only for features)
--
carsN :: Val Car Feature
carsN = valuation carsF

compare :: (Eq o,Ord a) => Info o a -> o -> o -> Rec a
compare m o1 o2 = select o1 m `diff` select o2 m


-- (3) Some variation: adding/deleting/modifying a feature attribute
--
toyota :: Feature -> Double
toyota Price  = 20000
toyota Safety = 50
toyota MPG    = 30

features2 :: Info Car Feature
features2 = addAlternative Toyota toyota carsF

features3 = delAttribute carsF Price

features4 = modAttribute carsF Price Honda 45000


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
-- carsUF = extend carsF users
--
-- carsWUF :: Val Car (Weight,User,Feature)
-- carsWUF = extend carsUF weights

cars :: Val Car (Feature,User,Weight)
-- cars = val carsF `extendBy` users `extendBy` weights
cars = mkOneTuple carsN `extendBy` users `extendBy` weights

carPriority :: Priority Car
carPriority = priority cars

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
vdCar = select Honda cars `diff` select BMW cars

exp0 :: Explain (Feature,User,Weight)
exp0 = explain vdCar

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
