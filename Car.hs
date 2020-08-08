{-# LANGUAGE  DeriveAnyClass #-}
module Car where

import Attribute
import Object
import Valuation
import MDS 

-- Car Example
--
data User    = Friend | Expert deriving (Eq,Ord,Show)
data Car     = Honda | BMW | Toyota deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Feature = Price | Fuel | Safety deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Weight = Weight deriving (Eq,Ord,Show)

instance AttrValence Feature where
   valence Price  = Neg
   valence Fuel   = Pos
   valence Safety = Pos


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

-- o7 = delDimension o3 [Fuel,Price,Safety]
