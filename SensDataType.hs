module SensDataType where

import qualified Data.Map as M 
import Record

data Sens a = Sens {unsens :: M.Map a (Maybe Double)}

mkSens :: Ord a => [(a,Maybe Double)] -> Sens a 
mkSens = Sens . M.fromList 

onSens :: Ord a => (M.Map a (Maybe Double) -> M.Map b (Maybe Double)) -> Sens a -> Sens b
onSens f = Sens . f . unsens

mapSens :: Ord b => ((a,Maybe Double) -> (b,Maybe Double)) -> Sens a -> Sens b 
mapSens f = Sens . M.fromList . map f . fromSens

fromSens :: Sens a -> [(a,Maybe Double)]
fromSens = M.toList . unsens 

-- show instance for Sens type
instance Show a => Show (Sens a) where
  show = showSens 2 ""

-- show record values as percentages
showSens :: Show a => Int -> String -> Sens a -> String 
showSens n s = showSet . map (showPairM n s) . fromSens

showPairM :: Show a => Int -> String -> (a,Maybe Double) -> String
showPairM n s (x,Just y') = showPairD n "" (x,-y')
showPairM _ _ (x,_)       = show x ++ " -> *"
     

-- ============================================================================
class Bound a where
    upperBound :: a -> Int
    upperBound _ = maxBound



