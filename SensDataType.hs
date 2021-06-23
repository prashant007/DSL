module SensDataType where

import qualified Data.Map as M 
import Record

data Change a = Change {unChange :: M.Map a (Maybe Double)}

mkChange :: Ord a => [(a,Maybe Double)] -> Change a 
mkChange = Change . M.fromList 

onChange :: Ord a => (M.Map a (Maybe Double) -> M.Map b (Maybe Double)) -> Change a -> Change b
onChange f = Change . f . unChange

mapChange :: Ord b => ((a,Maybe Double) -> (b,Maybe Double)) -> Change a -> Change b 
mapChange f = Change . M.fromList . map f . fromChange

fromChange :: Change a -> [(a,Maybe Double)]
fromChange = M.toList . unChange 

-- show instance for Change type
instance Show a => Show (Change a) where
  show = showChange 2 ""

-- show record values as percentages
showChange :: Show a => Int -> String -> Change a -> String 
showChange n s = showSet . map (showPairM n s) . fromChange

showPairM :: Show a => Int -> String -> (a,Maybe Double) -> String
showPairM n s (x,Just y') = showPairD n "" (x,-y')
showPairM _ _ (x,_)       = show x ++ " -> *"
     

-- ============================================================================
-- class Limit a where
--     maxVal :: a -> Int
--     maxVal _ = maxBound

--     minVal :: a -> Int 
--     minVal _ = minBound 

class Bound a where
    upperBound :: a -> Int
    upperBound _ = maxBound

