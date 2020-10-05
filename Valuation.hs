module Valuation where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List hiding (transpose)
import Text.Printf
import Data.Maybe
import Data.Tuple.OneTuple (only,OneTuple(..))

import Record
import Info
import Dimension


-- Distinguish beneficial and non-beneficial attributes
--
class Ord a => Valence a where
   valence :: a -> Bool
   valence _ = True


-- Valuation
--
type Val o a = Info o a

maxVal = 100

valuation :: (Ord o,Set a,Valence a) => Info o a -> Val o a
valuation i = (transpose . mkInfo) $ map (attrNormRecPair i) members
  where
    -- form pairs of attributes and records made from normalized Nums
    attrNormRecPair :: (Ord o,Valence a) => Info o a -> a -> (a,Rec o)
    attrNormRecPair l x = (x,mkRec $ normNums x l)

    -- normalize Nums obtained from an anttribute
    normNums :: (Ord o,Valence a) => a -> Info o a -> Nums o
    normNums x = normalize x . toNums x

    normalize :: Valence a => a -> Nums o -> Nums o
    normalize c as = let vs = [v | (_,v) <- as]
                         s = sum vs
                         s' = sum.map (\x -> 1/x) $ vs
                      in [(a,if valence c then (v/s)*maxVal else ((1/v)/s')*maxVal) | (a,v) <- as]


val :: (Set o,Valence a,Set a) => Info o a -> Val o (OneTuple a)
val = mkOneTuple . valuation

agg  :: Ord a => ([Double] -> Double) -> Val o a -> Rec o
agg f = Rec . M.map (f . M.elems . unRec) . unInfo

total :: Ord a => Val o a -> Rec o
total = agg sum

average :: Ord a => Val o a -> Rec o
average = agg (\xs->sum xs/fromIntegral (length xs))

mkOneTuple :: (Ord o,Ord a) => Val o a -> Val o (OneTuple a)
mkOneTuple = mapInfo $ onRec (M.mapKeys OneTuple)

extendBy :: (Ord o,Ord b,Valence c,Set c,Ord d,Expand a c d,Covers a b) => Val o a -> Info b c -> Val o d
extendBy as bs = listToInfo
                   [((o,expand aa cc),(av*cv)/maxVal) |
                    (o,a) <- fromInfo as,             (aa,av) <- fromRec a,
                    (b,c) <- (fromInfo.valuation) bs, (cc,cv) <- fromRec c,
                    project aa == b]
