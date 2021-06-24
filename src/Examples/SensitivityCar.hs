{-# LANGUAGE DeriveAnyClass #-}

module Examples.SensitivityCar where

import Encoding.Record
import Encoding.Info
import Encoding.Valuation
import Data.Tuple.OneTuple (OneTuple(..))

import Explanation.SensitivityAnalysis
import Explanation.SensDataType


data Car     = Honda | BMW | Toyota  deriving (Eq,Ord,Show,Enum,Bounded,Set)

data Feature = Price | MPG | Safety deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Opinion = Personal | Expert deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Weight  = Weighted deriving (Eq,Ord,Show,Enum,Bounded,Set)

instance Valence Feature where
  valence Price = False
  valence _     = True

instance Valence Opinion
instance Valence Weight

instance Limit Feature where
    upperBound Price = maxBound
    upperBound MPG   = 50
    upperBound Safety= 10

    lowerBound Price = 10000
    lowerBound MPG   = 10
    lowerBound Safety= 1

instance Limit Weight where
    upperBound Weighted = 100 

instance Limit Opinion

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

sensPrice = sensitivity carData (Honda,BMW) Price 
sensMPG   = sensitivity carData (Honda,BMW) MPG   
sensSafety= sensitivity carData (Honda,BMW) Safety 

sensPrice' = sensDefault carData cars Price  
sensMPG'   = sensDefault carData cars MPG 
sensSafety'= sensDefault carData cars Safety 

sensPrice_TH  = sensitivity carData (Toyota,Honda) Price 
sensMPG_TH    = sensitivity carData (Toyota,Honda) MPG   
sensSafety_TH = sensitivity carData (Toyota,Honda) Safety

sensPersonal= sensitivity carData (Honda,BMW) Personal 
sensExpert  = sensitivity carData (Honda,BMW) Expert 
sensPersonal_TH = sensitivity carData (Toyota,Honda) Personal 
sensExpert_TH   = sensitivity carData (Toyota,Honda) Expert 

c3TH   = sensDefault carData cars Weighted 
c3HB   = sensitivity carData (Honda,BMW) Weighted 

t = total $ cars 


