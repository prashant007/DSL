{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module Valuation where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List
import Text.Printf
import Data.Maybe
import Data.Tuple.OneTuple (only,OneTuple(..))

import Attribute
import Object


type Fraction = Double


-- Valuation
--
valuation :: (Set o,AttrValence a) => Obj o a -> Val o a
valuation o = addAllAttributesVal (\x -> (fromJust.lookup x) xs) (map fst xs) objects
  where
    xs = f o
    os =  nub.map fst.concatMap snd $ xs

    l :: AttrValence a => (a,Spread o) -> (a,Spread o)
    l (x,y) = (x,normalize x y)

    g :: (o,Attr a) -> [(a,(o,Fraction))]
    g (o,as) = map (\(a,v) -> (a,(o,v))) (fromAttr as)

    h :: Eq a => (a,b) -> (a,c) -> Bool
    h = \x y -> fst x == fst y

    k :: [(a,(o,Fraction))] -> (a,Spread o)
    k ls = (fst.head $ ls,map snd ls)

    f :: (Eq a,AttrValence a,Ord a) => Obj o a -> [(a,Spread o)]
    f = map (l.k) .groupBy h. sortBy (compare `on` fst). concatMap g.fromObj

val :: (Set o,AttrValence a) => Obj o a -> Val o (OneTuple a)
val = mkOneTuple . valuation

addAllAttributesVal :: (Ord a,Ord o) => (a -> Spread o) -> [a] -> Obj o a -> Obj o a
addAllAttributesVal f cs bs = foldl (\b c -> addAttributeVal c (f c) b) bs cs

addAttributeVal :: (Ord a,Ord o) => a -> Spread o -> Obj o a -> Obj o a
addAttributeVal c as bs = mkObj [(b,f c av bv) | (a,av) <- as,(b,bv) <- bs',a == b]
    where f x xv ys = Attr $ M.insert x xv (unAttr ys)
          bs' = fromObj bs

normalize :: AttrValence a => a -> Spread o -> Spread o
normalize c as = let vs = [v | (_,v) <- as]
                     s = sum vs
                     s' = sum.map (\x -> 1/x) $ vs 
                 in case valence c of
                      Pos -> [(a,v/s) | (a,v) <- as]
                      Neg -> [(a,(1/v)/s') | (a,v) <- as]


type Val o a = Obj o a

mkVal :: (Ord b,Ord o) => [(o,(b,Double))] -> Val o b
mkVal = mkObj.map f.groupBy h.sortBy (compare `on` fst)
  where
    f ls = (fst.head $ ls,mkAttr.map snd $ ls)
    h :: Eq a => (a,b) -> (a,c) -> Bool
    h = \x y -> fst x == fst y


mkOneTuple :: (Ord o,Ord a) => Obj o a -> Obj o (OneTuple a)
mkOneTuple = mkObj.map (\(o,a) -> (o,f a)).fromObj
  where
    f = mkAttr.map (\(b,n) -> (OneTuple b,n)).fromAttr

class (Projector a b,Ord d,Ord o,Set b,AttrValence c) => ExtendVal o a b c d | a b c -> d where
  mkTuple :: o -> (a,b,c) -> Double -> (o,(d,Double))

  extendBy :: Val o a -> Obj b c -> Val o d
  extendBy as bs = mkVal [mkTuple o (aa,b,cc) (av*cv) | (o,a) <- fromObj as, (aa,av) <- fromAttr a,
                        (b,c) <- (fromObj.valuation) bs, (cc,cv) <- fromAttr c, proj aa==b]

instance (Set a,AttrValence b,Ord o) => ExtendVal o (OneTuple a) a b (a,b) where
  mkTuple o (a,_,b) n = (o,((only a,b),n))

instance (Set b,AttrValence c,Ord o,Ord a) => ExtendVal o (a,b) b c (a,b,c) where
  mkTuple o ((a,b),_,c) n = (o,((a,b,c),n))

instance (Set c,AttrValence d,Ord o,Ord a,Ord b) => ExtendVal o (a,b,c) c d (a,b,c,d) where
  mkTuple o ((a,b,c),_,d) n = (o,((a,b,c,d),n))

type Priority o = [(o,Fraction)]

priority :: Obj o a -> Priority o
priority = map (\(o,a) -> (o,f a)).fromObj
  where
    f = sum.map snd.fromAttr

-- car,feature, feature user 

{-
addAlternative :: (Ord o,Ord a) => o -> (a -> Double) -> Obj o a -> Obj o a
addAlternative o f vs = Obj $ M.insert o (mkAttr ls) (unObj vs)
    where
        xs = (map fst.fromAttr.snd.head.fromObj) vs
        ls = map (\x -> (x,f x)) xs
-}
