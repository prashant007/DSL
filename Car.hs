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
data Weight = Weight deriving (Eq,Ord,Show,Enum,Bounded,Set)


instance AttrValence Feature where
   valence Price  = Neg
   valence Fuel   = Pos
   valence Safety = Pos

instance AttrValence User  

instance AttrValence Weight 

cars :: [Car]
cars = [Honda,BMW]

o0 :: Obj Car Feature
o0 = newDim

o1 :: Obj Car Feature
o1 = addAttribute Price [Honda --> 36000,BMW --> 24000] o0

o2 :: Obj Car Feature
o2 = addAttribute Safety [Honda --> 30,BMW --> 70] o1

o3 :: Obj Car Feature
o3 = addAttribute Fuel [Honda --> 36,BMW --> 24] o2

features :: Feature -> Spread Car
features x = case x of
                      Price  -> [Honda --> 36000,BMW --> 24000]
                      Safety -> [Honda --> 30,BMW --> 70]
                      Fuel   -> [Honda --> 36,BMW --> 24]

o3' :: Obj Car Feature
o3' = addDim features

v3 :: Obj Car Feature
v3 = valuation o3


toyotaAttributes :: Feature -> Double
toyotaAttributes x = case x of
        Price  -> 20000
        Safety -> 50
        Fuel   -> 30

o4 = addAlternative Toyota toyotaAttributes o3'

v4 :: Obj Car Feature
v4 = valuation o4

o5 = delAttribute o4 Price

o6 = modAttribute o5 Price Honda 45000


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

-- filter the trace component for bmw 
honda :: CarDecomp
honda = select Honda traceCar

-- filter the trace component for bmw 
bmw :: CarDecomp
bmw = select BMW traceCar

vdCar :: CarDecomp 
vdCar = diff honda bmw 

exp1 :: Explain User 
exp1 = generalize vdCar

exp2 :: Explain Feature 
exp2 = generalize vdCar

a0 :: Obj Feature User 
a0 = newDim

a1 :: Obj Feature User
a1 = addAttribute Friend [Price --> 0.5, Fuel --> 0.3, Safety --> 0.2] a0

a2 :: Obj Feature User
a2 = addAttribute Expert [Price --> 0.2, Fuel --> 0.4, Safety --> 0.4] a1

b1 :: Val Car (User,Feature)
b1 = extend v3 a2 

c0 :: Obj User Weight
c0 = newDim


c1 :: Obj User Weight 
c1 = addAttribute Weight [Friend --> 0.6,Expert --> 0.4] c0 

b2 :: Val Car (Weight,User,Feature)
b2 = extend b1 c1