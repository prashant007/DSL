{-# LANGUAGE FlexibleInstances #-}
module Focus where

import Data.Function
import qualified Data.Map as M
import Text.Printf
import Data.List hiding (filter)
import Prelude hiding (filter)
import Data.Tuple.OneTuple as T (only,OneTuple)
import Data.Maybe

import Record
import Info
import Classes
import MDS hiding (compare)


-- ========================== FACTORING =====================================
-- ==========================================================================

-- Consider the following record value: {(Friend,Safety) -> -0.048,(Expert,Safety) -> -0.064}
-- We observe that Safety could be factored in this Norm value giving us this pretty printed
-- representation: Safety: -0.112 (Friend:-0.048, Expert:-0.064). This factored representation
-- can be easier for the end user to consume than the original unfactored one. GroupBy type class
-- provides an interface to the factorize function which performs the factorization operation
-- and pFactor prints the factor in the pretty printed form.

data Focus k a  = Focus {unFocus :: M.Map k (Double,Rec a)}

onFocus :: (M.Map k (Double,Rec a) -> M.Map k (Double,Rec b)) -> Focus k a -> Focus k b
onFocus f = Focus . f . unFocus

mapFocus :: ((Double,Rec a) -> (Double,Rec b)) -> Focus k a -> Focus k b
mapFocus f = Focus . M.map f . unFocus

mapFocusWithKey :: (k -> (Double,Rec a) -> (Double,Rec b)) -> Focus k a -> Focus k b
mapFocusWithKey f = Focus . M.mapWithKey f . unFocus

foldFocus :: (b -> (Double,Rec a) -> b) -> b -> Focus k a -> b
foldFocus f a x = M.foldl f a (unFocus x)

foldFocusWithKey :: (b -> k -> (Double,Rec a) -> b) -> b -> Focus k a -> b
foldFocusWithKey f a x = M.foldlWithKey f a (unFocus x)

lookUpFocus :: Ord a => a -> Focus a b -> Double
lookUpFocus x  = fst . fromJust . M.lookup x . unFocus

instance {-# OVERLAPPING #-} Show k => Show (Focus k ()) where
  show = foldFocusWithKey pFocus ""
    where
      pFocus :: (Show k,Show a) => String -> k -> (Double,Rec a) -> String
      pFocus x k (v,r) =  x ++ "\n "  ++ show k ++ " : " ++ printf "%.0f" v
                          ++ "%"

instance {-# OVERLAPPING #-} (Show k,Show a) => Show (Focus k a) where
  show = foldFocusWithKey pFocus ""
    where
      pFocus :: (Show k,Show a) => String -> k -> (Double,Rec a) -> String
      pFocus x k (v,r) =  x ++ "\n "  ++ show k ++ " : " ++ printf "%.0f" v
                          ++ "% " ++ showRecPercent r


-- this changes focused values from absolutes values to percentages
formatFocus :: Ord a => Focus k a -> Focus k a
formatFocus x = mapFocus percentFocus x
  where
    sumF = foldFocus (\b (n,_) -> b + abs n) 0 x

    percentFocus :: Ord a => ((Double,Rec a) -> (Percent,Rec a))
    percentFocus (x,y) = (mkPercent sumF x,percentRec y)
