{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,DataKinds #-}

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


addAllAttrVal :: (Ord a,Ord o) => (a -> Nums o) -> [a] -> Info o a -> Val o a
addAllAttrVal f cs bs = foldl (\b c -> addAttrVal c (f c) b) bs cs

addAttrVal :: (Ord a,Ord o) => a -> Nums o -> Info o a -> Info o a
addAttrVal c as bs = mkInfo [(b,f c av bv) | (a,av) <- as,(b,bv) <- bs',a == b]
    where f x xv ys = Rec $ M.insert x xv (unRec ys)
          bs' = fromInfo bs


crtVal :: (Ord b,Ord o) => [(o,(b,Double))] -> Val o b
crtVal = mkInfo.map f.groupBy h.sortBy (compare `on` fst)
  where
    f ls = (fst.head $ ls, mkRec . map snd $ ls)
    h :: Eq a => (a,b) -> (a,c) -> Bool
    h = \x y -> fst x == fst y


mkOneTuple :: (Ord o,Ord a) => Val o a -> Val o (OneTuple a)
mkOneTuple = mkInfo . map (\(o,a) -> (o,f a)) . fromInfo
  where
    f = mkRec . map (\(b,n) -> (OneTuple b,n)) . fromRec

class (Projector a b,Ord d,Ord o,Set b,Set c,Valence c) => ExtendVal o a b c d | a b c -> d where
  mkTuple :: o -> (a,b,c) -> Double -> (o,(d,Double))

  extend :: Val o a -> (c -> Nums b) -> Val o d
  extend as f = extendBy as (gather f)

  extendBy :: Val o a -> Info b c -> Val o d
  extendBy as bs = crtVal [mkTuple o (aa,b,cc) ((av*cv)/maxVal) | (o,a) <- fromInfo as, (aa,av) <- fromRec a,
                           (b,c) <- (fromInfo.valuation) bs, (cc,cv) <- fromRec c, proj aa==b]

instance (Set a,Set b,Valence b,Ord o) => ExtendVal o (OneTuple a) a b (a,b) where
  mkTuple o (a,_,b) n = (o,((only a,b),n))

instance (Set b,Set c,Valence c,Ord o,Ord a) => ExtendVal o (a,b) b c (a,b,c) where
  mkTuple o ((a,b),_,c) n = (o,((a,b,c),n))

instance (Set c,Set d,Valence d,Ord o,Ord a,Ord b) => ExtendVal o (a,b,c) c d (a,b,c,d) where
  mkTuple o ((a,b,c),_,d) n = (o,((a,b,c,d),n))

type Priority o = [(o,Double)]

priority :: Val o a -> Priority o
priority = map (\(o,a) -> (o,f a)).fromInfo
  where
    f = sum.map snd . fromRec
