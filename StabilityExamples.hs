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


data Car     = Honda | BMW  deriving (Eq,Ord,Show,Enum,Bounded,Set)

data Feature = Price | MPG | Safety deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Opinion = Personal | Expert deriving (Eq,Ord,Show,Enum,Bounded,Set)
data Weight  = Weighted deriving (Eq,Ord,Show,Enum,Bounded,Set)

instance Valence Feature where
  valence Price = False
  valence _     = True

instance Valence Opinion
instance Valence Weight


carFeatures :: Info Car Feature
carFeatures = info [Honda --> [Price --> (34000 ), MPG --> 30, Safety --> 9.8],
                    BMW   --> [Price --> 36000, MPG --> 32, Safety --> 9.1]]


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
weights = info [Personal --> weight 0.6,Expert --> weight 0.4]

cars :: Val Car (Feature,Opinion,Weight)
cars = carOpinions `extendBy` info [Personal --> weight (0.6),Expert --> weight 0.4]

carTuple = (carFeatures,featureOpinions,weights)
c11 = sens carTuple (Honda,BMW) :: Sens Car (Car,Feature) 
c12 = sens carTuple (BMW,Honda) :: Sens Car (Car,Feature) 
c2  = sens carTuple (Honda,BMW) :: Sens Car (Feature,Opinion)
c3  = sens carTuple (Honda,BMW) :: Sens Car (Opinion,Weight) 

t = total $ cars 



-- ================== Examples from Evangelos's Book ====================== 

data Alt = A1 | A2 | A3 | A4 deriving (Eq,Ord,Show,Enum,Bounded,Set)

data C = C1 | C2 | C3 | C4 deriving (Eq,Ord,Show,Enum,Bounded,Set)

data W = W deriving (Eq,Ord,Show,Enum,Bounded,Set)


instance Valence Alt
instance Valence C 
instance Valence W 

firstLevel :: Val Alt C 
firstLevel = info [A1 --> [C1 --> 0.3088, C2 --> 0.2897, C3 --> 0.3867, C4 --> 0.1922],
                   A2 --> [C1 --> 0.2163, C2 --> 0.3458, C3 --> 0.1755, C4 --> 0.6288],
                   A3 --> [C1 --> 0.4509, C2 --> 0.2473, C3 --> 0.1194, C4 --> 0.0575],
                   A4 --> [C1 --> 0.0240, C2 --> 0.1172, C3 --> 0.3184, C4 --> 0.1215]] 

secondLevel :: Val C W 
secondLevel = info [C1 --> [W --> 0.3277],C2 --> [W --> 0.3058],
                    C3 --> [W --> 0.2876],C4 --> [W --> 0.0790]]



-- data Alt = A1 | A2 | A3 | A4 | A5 deriving (Eq,Ord,Show,Enum,Bounded,Set)

-- data C = C1 | C2 | C3 | C4 | C5 deriving (Eq,Ord,Show,Enum,Bounded,Set)

-- data W = W deriving (Eq,Ord,Show,Enum,Bounded,Set)


-- instance Valence Alt
-- instance Valence C 
-- instance Valence W 

-- firstLevel :: Val Alt C 
-- firstLevel = info [A1 --> [C1 --> 0.3576, C2 --> 0.2483, C3 --> 0.2899, C4 --> 0.2961, C5 --> 0.3202],
--                    A2 --> [C1 --> 0.3603, C2 --> 0.2836, C3 --> 0.0407, C4 --> 0.0939, C5 --> (0.0172)], 
--                    A3 --> [C1 --> 0.0255, C2 --> 0.1745, C3 --> 0.2895, C4 --> 0.2212, C5 --> 0.2641], 
--                    A4 --> [C1 --> 0.1609, C2 --> 0.2008, C3 --> 0.2960, C4 --> 0.0716, C5 --> 0.0315], 
--                    A5 --> [C1 --> 0.0957, C2 --> 0.0928, C3 --> 0.0839, C4 --> 0.3172, C5 --> 0.3670]] 


-- secondLevel :: Val C W 
-- secondLevel = info [C1 --> [W --> 0.4146],C2 --> [W --> 0.0129],C3 --> [W --> 0.2958],
--                     C4 --> [W --> 0.0604],C5 --> [W --> 0.2164]]



type Level = Int 

allSens1 :: Sval o (Info2 o a b) o a => Info2 o a b -> [Sens o (o,a)]
allSens1 v =  map (\x -> (sens' v x)) (genPairs (/=))
    -- | otherwise = Right $ map (\x -> (sens v x)::Sens o (a,b)) (genPairs (<))


allSens2 :: (Ord o,Set o,Sval o (Info2 o a b) a b) => Info2 o a b -> [Sens o (a,b)]
allSens2 v =  map (\x -> (sens' v x)) (genPairs (<))

genPairs :: Set o => (o -> o -> Bool) ->  [(o,o)]
genPairs f = [(x,y) | x <- members, y <- members, f x y]
        
pSens :: (Show o,Show a) => [Sens o a]-> IO ()
pSens = mapM_ (\x -> putStrLn $ show x ++ "\n") 


