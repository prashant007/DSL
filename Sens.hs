module Sens where

import qualified Data.Map as M 
import Data.Tuple.OneTuple (OneTuple(..))
import qualified Data.List as L 
import GHC.TypeLits

import Record
import Info
import Valuation hiding (val)
import MDS
import Transformation
import Dimension

type Pair o = (o,o)

data Sens o a = Sens {unpair :: Pair o, unsens :: M.Map a (Maybe Double)}

mkSens :: Ord a => Pair o -> [(a,Maybe Double)] -> Sens o a 
mkSens p = Sens p . M.fromList 

mapSens :: Ord b => ((a,Maybe Double) -> (b,Maybe Double)) -> Sens o a -> Sens o b 
mapSens f =  (\(x,y) -> mkSens x $ map f y) . fromSens

fromSens :: Sens o a -> (Pair o,[(a,Maybe Double)])
fromSens x = (unpair x,M.toList . unsens $ x)

-- show instance for Sens type
instance (Show o,Show a) => Show (Sens o a) where
  show = showSens 4 ""

-- show record values as percentages
showSens :: (Show o,Show a) => Int -> String -> Sens o a -> String 
showSens n s p = (show . fst) p' ++ " : " ++ (showSet . map (showPairM n s) . snd) p'
    where p' = fromSens p 

showPairM :: Show a => Int -> String -> (a,Maybe Double) -> String
showPairM n s (x,Just y') = showPairD n "" (x,y')
showPairM _ _ (x,_)       = show x ++ " -> *"
     

        