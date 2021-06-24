{-# LANGUAGE FlexibleInstances #-}
module Explanation.Factor where

import qualified Data.Map as M
import Text.Printf
import Prelude hiding (filter)
import Data.Maybe

import Encoding.Record
import Encoding.Info
import Encoding.Dimension


-- ========================== FACTORING =====================================
-- ==========================================================================

-- Consider the following record value: {(Friend,Safety) -> -0.048,(Expert,Safety) -> -0.064}
-- We observe that Safety could be factored in this Norm value giving us this pretty printed
-- representation: Safety: -0.112 (Friend:-0.048, Expert:-0.064). This factored representation
-- can be easier for the end user to consume than the original unfactored one. GroupBy type class
-- provides an interface to the factorize function which performs the factorization operation
-- and pFactor prints the factor in the pretty printed form.

data Factor k a  = Factor {unFactor :: M.Map k (Double,Rec a)}

onFactor :: (M.Map k (Double,Rec a) -> M.Map k (Double,Rec b)) -> Factor k a -> Factor k b
onFactor f = Factor . f . unFactor

mapFactor :: ((Double,Rec a) -> (Double,Rec b)) -> Factor k a -> Factor k b
mapFactor f = Factor . M.map f . unFactor

mapFactorWithKey :: (k -> (Double,Rec a) -> (Double,Rec b)) -> Factor k a -> Factor k b
mapFactorWithKey f = Factor . M.mapWithKey f . unFactor

foldFactor :: (b -> (Double,Rec a) -> b) -> b -> Factor k a -> b
foldFactor f a x = M.foldl f a (unFactor x)

foldFactorWithKey :: (b -> k -> (Double,Rec a) -> b) -> b -> Factor k a -> b
foldFactorWithKey f a x = M.foldlWithKey f a (unFactor x)

lookUpFactor :: Ord a => a -> Factor a b -> Double
lookUpFactor x  = fst . fromJust . M.lookup x . unFactor

fromFactor :: Factor k a -> [(k,(Double,Rec a))]
fromFactor = M.toList . unFactor

instance {-# OVERLAPPING #-} Show k => Show (Factor k ()) where
  show = showSetLn . map (\(k,(v,_)) -> showFPair (k,v)) . fromFactor

instance {-# OVERLAPPING #-} (Show k,Show a) => Show (Factor k a) where
  show = showSetLn . map showF . fromFactor
    where
        showF (k,(v,r)) = showFPair (k,v) ++ " " ++ showRec 0 "%" r


showFPair :: Show k => (k,Double) -> String
showFPair (k,v) = show k ++ " -> " ++ printf "%.0f" v ++ "%"

-- this changes focused values from absolutes values to percentages
formatFactor :: Ord a => Factor k a -> Factor k a
formatFactor x = mapFactor percentFactor x
  where
    sumF = foldFactor (\b (n,_) -> b + abs n) 0 x
    percentFactor :: Ord a => ((Double,Rec a) -> (Percent,Rec a))
    percentFactor (x,y) = (mkPercent sumF x,percentRec y)


