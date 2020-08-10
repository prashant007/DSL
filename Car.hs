{-# LANGUAGE  DeriveAnyClass #-}
module Car where

import Attribute
import Object
import Valuation
import MDS

import qualified Data.Map as M

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


-- (1) collecting car features
--
featuresP :: Obj Car Feature
featuresP = addAttribute Price [Honda --> 36000,BMW --> 24000] objects

featuresS :: Obj Car Feature
featuresS = addAttribute Safety [Honda --> 30,BMW --> 70] featuresP

featuresF :: Obj Car Feature
featuresF = addAttribute Fuel [Honda --> 36,BMW --> 24] featuresS

-- Alternative: doing it in one step
--
featureInfo :: Feature -> Spread Car
featureInfo x = case x of Price  -> [Honda --> 24000,BMW --> 36000]
                          Safety -> [Honda --> 30,BMW --> 70]
                          Fuel   -> [Honda --> 36,BMW --> 24]

features :: Obj Car Feature
features = gather featureInfo


-- (2) creating a valuation from data
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

-- (5) Creating valuations for extended data
--
-- carsUF :: Val Car (User,Feature)
-- carsUF = extend carsF users
--
-- carsWUF :: Val Car (Weight,User,Feature)
-- carsWUF = extend carsUF weights

carsVal :: Val Car (Weight,User,Feature)
carsVal = carsF `extend` users `extend` weights

-- to be removed ...
--
traceCar = mkObj [(Honda,mkAttr [((Weight,Friend,Price),0.180),((Weight,Friend,Fuel),0.108),((Weight,Friend,Safety),0.036),
                                 ((Weight,Expert,Price),0.048),((Weight,Expert,Fuel),0.096),((Weight,Expert,Safety),0.048)
                                ]),
                  (BMW,mkAttr [((Weight,Friend,Price),0.120),((Weight,Friend,Fuel),0.072),((Weight,Friend,Safety),0.084),
                               ((Weight,Expert,Price),0.032),((Weight,Expert,Fuel),0.064),((Weight,Expert,Safety),0.112)
                              ])
                 ]

-- ======================= FILTERING ELEMENTS FROM ANNOTATED VALUES ===========================
-- ============================================================================================

type CarDecomp = Attr (Weight,User,Feature)

-- filter the valuation for specific cars

honda :: CarDecomp
honda = select Honda carsVal

bmw :: CarDecomp
bmw = select BMW carsVal

vdCar :: CarDecomp
vdCar = diff honda bmw

exp0 :: Explain (Weight,User,Feature) 
exp0 = explain vdCar

showMDSexp0 = pmds exp0
showDomexp0 = pdom exp0

exp1 :: Explain User
exp1 = generalize vdCar

exp2 :: Explain Feature
exp2 = generalize vdCar

