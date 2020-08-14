{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module Valuation where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List
import Text.Printf
import Data.Maybe
import Data.Tuple.OneTuple(only,OneTuple)

import Attribute
import Object


type Fraction = Double


-- Valuation
--
valuation :: (Set o,AttrValence a) => Obj o a -> Val o a
valuation xs = addAllAttributesVal (\x -> (fromJust.lookup x) xs') (map fst xs') objects
  where
    xs' = f xs
    os =  nub.map fst.concatMap snd $ xs'

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

addAllAttributesVal :: (Ord a,Ord o) => (a -> Spread o) -> [a] -> Obj o a -> Obj o a
addAllAttributesVal f cs bs = foldl (\b c -> addAttributeVal c (f c) b) bs cs

addAttributeVal :: (Ord a,Ord o) => a -> Spread o -> Obj o a -> Obj o a
addAttributeVal c as bs = mkObj [(b,f c av bv) | (a,av) <- as,(b,bv) <- bs',a == b]
    where f x xv ys = Attr $ M.insert x xv (unAttr ys)
          bs' = fromObj bs

normalize :: AttrValence a => a -> Spread o -> Spread o
normalize c as = let s = sum [v | (_,v) <- as]
                 in case valence c of
                      Pos -> [(a,v/s) | (a,v) <- as]
                      Neg -> [(a,1-v/s) | (a,v) <- as]


type Val o a = Obj o a

mkVal :: (Ord b,Ord o) => [(o,(b,Double))] -> Val o b
mkVal = mkObj.map f.groupBy h.sortBy (compare `on` fst)
  where
    f ls = (fst.head $ ls,mkAttr.map snd $ ls)
    h :: Eq a => (a,b) -> (a,c) -> Bool
    h = \x y -> fst x == fst y

class (Projector a b,Ord d,Ord o,Set b,AttrValence c) => ExtendVal o a b c d | a b c -> d where
  mkTuple :: o -> (a,b,c) -> Double -> (o,(d,Double)) 

  extend :: Val o a -> Obj b c -> Val o d
  extend as bs = mkVal [mkTuple o (aa,b,cc) (av*cv) | (o,a) <- fromObj as, (aa,av) <- fromAttr a,
                        (b,c) <- (fromObj.valuation) bs, (cc,cv) <- fromAttr c, proj aa==b]

instance (Set a,AttrValence b,Ord o) => ExtendVal o (OneTuple a) a b (b,a) where
  mkTuple o (a,_,b) n = (o,((b,only a),n))

instance (Set a,AttrValence c,Ord o,Ord b) => ExtendVal o (a,b) a c (c,a,b) where
  mkTuple o ((a,b),_,c) n = (o,((c,a,b),n))

instance (Set a,AttrValence d,Ord o,Ord b,Ord c) => ExtendVal o (a,b,c) a d (d,a,b,c) where
  mkTuple o ((a,b,c),_,d) n = (o,((d,a,b,c),n))


{-
addAlternative :: (Ord o,Ord a) => o -> (a -> Double) -> Obj o a -> Obj o a
addAlternative o f vs = Obj $ M.insert o (mkAttr ls) (unObj vs)
    where
        xs = (map fst.fromAttr.snd.head.fromObj) vs
        ls = map (\x -> (x,f x)) xs
-}
