{-# LANGUAGE DeriveAnyClass #-}

import Record
import Info
import Valuation
import Data.Tuple.OneTuple (OneTuple(..))
import SensitivityAnalysis
import SensDataType


data Car     = Honda | BMW | Toyota  deriving (Eq,Ord,Show,Enum,Bounded,Set)

data Feature = Price | MPG | Safety deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Opinion = Personal | Expert deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Weight  = Weighted deriving (Eq,Ord,Show,Enum,Bounded,Set)

instance Valence Feature where
  valence Price = False
  valence _     = True

instance Valence Opinion
instance Valence Weight

instance Bound Feature where
    upperBound Price = maxBound
    upperBound MPG   = 50
    upperBound Safety= 10

instance Bound Weight where
    upperBound Weighted = 100 

instance Bound Opinion

-- change back valence 
carFeatures :: Info Car Feature
carFeatures = info [Honda --> [Price --> 34000, MPG --> 30, Safety --> 9.8],
                    BMW   --> [Price --> 36000, MPG --> 32, Safety --> 9.1],
                    Toyota--> [Price --> 27000, MPG --> 28, Safety --> 8.1]]


vCarF :: Val Car Feature
vCarF = valuation carFeatures


featureOpinions :: Info Feature Opinion
featureOpinions = info [Price  --> [Personal --> 5, Expert --> 3],
                        MPG    --> [Personal --> 3, Expert --> 5],
                        Safety --> [Personal --> 2, Expert --> 2]]

featureVal :: Val Car (OneTuple Feature)
featureVal = mkOneTuple vCarF

carOpinions :: Val Car (Feature,Opinion)
carOpinions = featureVal `extendBy` featureOpinions

weight :: a -> [(Weight,a)]
weight x = [Weighted --> x]

weights :: Info Opinion Weight
weights = info [Personal --> weight 60,Expert --> weight 40]

cars :: Val Car (Feature,Opinion,Weight)
cars = carOpinions `extendBy` weights

carData = (carFeatures,featureOpinions,weights)
c11HB = sensitivity carData (Honda,BMW) Price 
c12HB = sensitivity carData (Honda,BMW) MPG   
c13HB = sensitivity carData (Honda,BMW) Safety 

c11THD= sensDefault carData cars Price  
c12THD= sensDefault carData cars MPG 
c13THD= sensDefault carData cars Safety 

c11TH = sensitivity carData (Toyota,Honda) Price 
c12TH = sensitivity carData (Toyota,Honda) MPG   
c13TH = sensitivity carData (Toyota,Honda) Safety

c21HB  = sensitivity carData (Honda,BMW) Personal 
c22HB  = sensitivity carData (Honda,BMW) Expert 
c21TH  = sensitivity carData (Toyota,Honda) Personal 
c22TH  = sensitivity carData (Toyota,Honda) Expert 


c3TH   = sensDefault carData cars Weighted 
c3HB   = sensitivity carData (Honda,BMW) Weighted 

t = total $ cars 


