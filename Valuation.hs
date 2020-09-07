{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module Valuation where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List
import Text.Printf
import Data.Maybe
import Data.Tuple.OneTuple (only,OneTuple(..))

import Record
import Info


type Fraction = Double


-- Valuation
--
valuation :: (Set o,AttrValence a) => Info o a -> Val o a
valuation o = addAllAttributesVal (\x -> (fromJust.lookup x) xs) (map fst xs) objects
  where
    xs = f o
    os =  nub.map fst.concatMap snd $ xs

    l :: AttrValence a => (a,Spread o) -> (a,Spread o)
    l (x,y) = (x,normalize x y)

    g :: (o,Rec a) -> [(a,(o,Fraction))]
    g (o,as) = map (\(a,v) -> (a,(o,v))) (fromRec as)

    h :: Eq a => (a,b) -> (a,c) -> Bool
    h = \x y -> fst x == fst y

    k :: [(a,(o,Fraction))] -> (a,Spread o)
    k ls = (fst.head $ ls,map snd ls)

    f :: (Eq a,AttrValence a,Ord a) => Info o a -> [(a,Spread o)]
    f = map (l.k) .groupBy h. sortBy (compare `on` fst). concatMap g.fromInfo

val :: (Set o,AttrValence a) => Info o a -> Val o (OneTuple a)
val = mkOneTuple . valuation


val' :: (Set o,Set a,AttrValence a) => (a -> Spread o)  -> Val o (OneTuple a)
val' = (val.gather)

addAllAttributesVal :: (Ord a,Ord o) => (a -> Spread o) -> [a] -> Info o a -> Info o a
addAllAttributesVal f cs bs = foldl (\b c -> addAttributeVal c (f c) b) bs cs

addAttributeVal :: (Ord a,Ord o) => a -> Spread o -> Info o a -> Info o a
addAttributeVal c as bs = mkInfo [(b,f c av bv) | (a,av) <- as,(b,bv) <- bs',a == b]
    where f x xv ys = Rec $ M.insert x xv (unRec ys)
          bs' = fromInfo bs

normalize :: AttrValence a => a -> Spread o -> Spread o
normalize c as = let vs = [v | (_,v) <- as]
                     s = sum vs
                     s' = sum.map (\x -> 1/x) $ vs
                 in case valence c of
                      Pos -> [(a,v/(sum vs)) | (a,v) <- as]
                      Neg -> [(a,(1/v)/s') | (a,v) <- as]


type Val o a = Info o a

mkVal :: (Ord b,Ord o) => [(o,(b,Double))] -> Val o b
mkVal = mkInfo.map f.groupBy h.sortBy (compare `on` fst)
  where
    f ls = (fst.head $ ls, mkRec . map snd $ ls)
    h :: Eq a => (a,b) -> (a,c) -> Bool
    h = \x y -> fst x == fst y


mkOneTuple :: (Ord o,Ord a) => Info o a -> Info o (OneTuple a)
mkOneTuple = mkInfo . map (\(o,a) -> (o,f a)).fromInfo
  where
    f = mkRec . map (\(b,n) -> (OneTuple b,n)) . fromRec

class (Projector a b,Ord d,Ord o,Set b,Set c,AttrValence c) => ExtendVal o a b c d | a b c -> d where
  mkTuple :: o -> (a,b,c) -> Double -> (o,(d,Double))

  extend :: Val o a -> (c -> Spread b) -> Val o d
  extend as f = extendBy as (gather f)

  extendBy :: Val o a -> Info b c -> Val o d
  extendBy as bs = mkVal [mkTuple o (aa,b,cc) (av*cv) | (o,a) <- fromInfo as, (aa,av) <- fromRec a,
                        (b,c) <- (fromInfo.valuation) bs, (cc,cv) <- fromRec c, proj aa==b]

instance (Set a,Set b,AttrValence b,Ord o) => ExtendVal o (OneTuple a) a b (a,b) where
  mkTuple o (a,_,b) n = (o,((only a,b),n))

instance (Set b,Set c,AttrValence c,Ord o,Ord a) => ExtendVal o (a,b) b c (a,b,c) where
  mkTuple o ((a,b),_,c) n = (o,((a,b,c),n))

instance (Set c,Set d,AttrValence d,Ord o,Ord a,Ord b) => ExtendVal o (a,b,c) c d (a,b,c,d) where
  mkTuple o ((a,b,c),_,d) n = (o,((a,b,c,d),n))

type Priority o = [(o,Fraction)]

priority :: Info o a -> Priority o
priority = map (\(o,a) -> (o,f a)).fromInfo
  where
    f = sum.map snd . fromRec

-- car,feature, feature user

{-
addAlternative :: (Ord o,Ord a) => o -> (a -> Double) -> Info o a -> Info o a
addAlternative o f vs = Info $ M.insert o (mkRec ls) (unInfo vs)
    where
        xs = (map fst . fromRec.snd.head.fromInfo) vs
        ls = map (\x -> (x,f x)) xs
-}
