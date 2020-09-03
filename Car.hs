{-# LANGUAGE  DeriveAnyClass #-}
module Car where

import qualified Data.Map as M

import Attribute
import Object
import Valuation
import MDS
import Transformation
import Data.Tuple.OneTuple (only,OneTuple)


-- Car Example
--
data User    = Friend | Expert deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Car     = Honda | BMW | Toyota deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Feature = Price | Fuel | Safety deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Weight  = Weight deriving (Eq,Ord,Show,Enum,Bounded,Set)

instance AttrValence Feature where
   valence Price  = Neg
   valence Fuel   = Pos
   valence Safety = Pos

instance AttrValence User
instance AttrValence Weight


-- (1) Collecting car features
--
featuresP :: Obj Car Feature
featuresP = addAttribute Price [Honda --> c1P,BMW --> c2P] objects

featuresS :: Obj Car Feature
featuresS = addAttribute Safety [Honda --> c1S,BMW --> c2S] featuresP

featuresF :: Obj Car Feature
featuresF = addAttribute Fuel [Honda --> c1M,BMW --> c2M] featuresS

-- Alternative: doing it in one step
--
featureInfo :: Feature -> Spread Car
featureInfo x = case x of Price  -> [Honda --> c1P,BMW --> c2P]
                          Safety -> [Honda --> c1S,BMW --> c2S]
                          Fuel   -> [Honda --> c1M,BMW --> c2M]

features :: Obj Car Feature
features = gather featureInfo


-- (2) creating a valuation from data (only for features)
--
carsF :: Val Car Feature
carsF = valuation features


-- (3) Some variation: adding/deleting/modifying a feature attribute
--
toyotaAttributes :: Feature -> Double
toyotaAttributes x = case x of
        Price  -> 20000
        Safety -> 50
        Fuel   -> 30

features2 :: Obj Car Feature
features2 = addAlternative Toyota toyotaAttributes features

features3 = delAttribute features Price

features4 = modAttribute features Price Honda 45000


-- (4) Adding dimensions to car features (users and weights)
--
userInfo :: User -> Spread Feature
userInfo x = case x of Friend -> [Price --> 0.5, Fuel --> 0.3, Safety --> 0.2]
                       Expert -> [Price --> 0.2, Fuel --> 0.4, Safety --> 0.4]

users :: Obj Feature User
users = gather userInfo

weights :: Obj User Weight
weights = addAttribute Weight [Friend --> 0.6,Expert --> 0.4] objects



bAttr :: Attr Feature
bAttr = mkAttr [Price --> 36000, Fuel --> 24, Safety --> 70]

bObj :: Obj Car Feature 
bObj = mkObj [BMW --> bAttr]

-- (5) Creating valuations for extended data
--
-- carsUF :: Val Car (User,Feature)
-- carsUF = extend carsF users
--
-- carsWUF :: Val Car (Weight,User,Feature)
-- carsWUF = extend carsUF weights

cars :: Val Car (Feature,User,Weight)
cars = val features `extendBy` users `extendBy` weights

carPriority :: Priority Car 
carPriority = priority cars

-- ======================= FILTERING ELEMENTS FROM ANNOTATED VALUES ===========================
-- ============================================================================================

-- (6) Explaining decisions
--
type CarDecomp = Attr (Feature,User,Weight)

-- Valuations for specific cars
--
honda :: CarDecomp
honda = select Honda cars

bmw :: CarDecomp
bmw = select BMW cars

vdCar :: CarDecomp
vdCar = diff honda bmw

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
       in  reduce (head ms)::Attr (Feature,User)

m01 = pFact (factorize mds0 :: Factor Feature User)
m02 = pFact (factorize mds0 :: Factor User Feature)
-- m11 = factorize mds1 :: Factor Weight (User,Feature)


(c1N,c1P,c1M,c1S) = (Honda,34000,30,9.8)

(c2N,c2P,c2M,c2S) = (BMW,36000,33,9.1)