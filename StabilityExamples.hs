{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}



import Record
import Info
import Valuation
import Data.Tuple.OneTuple (OneTuple(..))
import MDS
import Factor
import Transformation
import Stability
import Sens 
import StabilityHelper 


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
-- carFeatures :: Info Car Feature
-- carFeatures = info [Honda --> [Price --> (40), MPG --> 30, Safety --> 9.8],
--                     BMW   --> [Price --> (36000), MPG --> 32, Safety --> 9.1],
--                     Toyota--> [Price --> (24000), MPG --> 28, Safety --> 8.1]]


-- change back valence 
carFeatures :: Info Car Feature
carFeatures = info [Honda --> [Price --> (34000), MPG --> 30, Safety --> 9.8],
                    BMW   --> [Price --> (36000), MPG --> 32, Safety --> 9.1],
                    Toyota--> [Price --> (24000), MPG --> 28, Safety --> 8.1]]



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

 --info [Personal --> weight (0.6),Expert --> weight 0.4]

carData = (carFeatures,featureOpinions,weights)
c11HB = sens carData (Honda,BMW) Price :: Sens Car 
c12HB = sens carData (Honda,BMW) MPG   :: Sens Car
c13HB = sens carData (Honda,BMW) Safety :: Sens Car

c11TH = sens carData (Toyota,Honda) Price :: Sens Car 
c11THD= sensdef carData Price cars :: Sens Car 
c12TH = sens carData (Toyota,Honda) MPG   :: Sens Car
c12THD= sensdef carData MPG cars :: Sens Car 
c13TH = sens carData (Toyota,Honda) Safety:: Sens Car
c13THD= sensdef carData Safety cars :: Sens Car 

c21HB  = sens carData (Honda,BMW) Personal :: Sens Feature
c22HB  = sens carData (Honda,BMW) Expert :: Sens Feature
c21TH  = sens carData (Toyota,Honda) Personal :: Sens Feature
c22TH  = sens carData (Toyota,Honda) Expert :: Sens Feature


c3TH   = sensdef carData Weighted cars :: Sens Opinion
c3HB   = sens carData (Honda,BMW) Weighted :: Sens Opinion
-- c12 = sens carData (BMW,Honda) :: Sens Car (Car,Feature) 
-- c2  = sens carData (Honda,BMW) :: Sens Car (Feature,Opinion)
-- c3  = sens carData (Honda,BMW) :: Sens Car (Opinion,Weight) 

t = total $ cars 




