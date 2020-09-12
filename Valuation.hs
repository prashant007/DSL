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


-- Used to represent beneficial or non-beneficial attributes
--
class Ord a => Valence a where
   valence :: a -> Bool
   valence _ = True


-- Valuation
--

-- type Val o a = Info o a
-- data Info o a = Info {unInfo :: M.Map o (Rec a)}

data Val o a = Val {unVal :: M.Map o (Rec a)}

instance (Show o,Show a) => Show (Val o a) where
  show ts = g $ map (\(x,y) -> show x ++ " ->\n" ++ f y) (fromVal ts)
      where g x = "{" ++ intercalate ",\n " x ++ "}\n"
            f x = g $ map (\(x,y) -> show x ++ " -> " ++ printf "%.1f" (y*100)) (fromRec x)

mkVal :: Ord o => [(o,Rec a)] -> Val o a
mkVal = Val . M.fromList

fromVal :: Val o a -> [(o,Rec a)]
fromVal = M.toList . unVal

infoToVal :: Info o a -> Val o a 
infoToVal =  Val . unInfo

select :: Eq o => o -> Val o a -> Rec a
select o = fromJust . lookup o . fromVal

(!) :: Eq o => Val o a -> o -> Rec a
(!) = flip select


valuation :: (Set o,Valence a) => Info o a -> Val o a
valuation o = addAllAttrVal (\x -> (fromJust.lookup x) xs) (map fst xs) objects
  where
    xs = f o
    os = nub . map fst . concatMap snd $ xs

    l :: Valence a => (a,Spread o) -> (a,Spread o)
    l (x,y) = (x,normalize x y)

    g :: (o,Rec a) -> [(a,(o,Fraction))]
    g (o,as) = map (\(a,v) -> (a,(o,v))) (fromRec as)

    h :: Eq a => (a,b) -> (a,b) -> Bool
    h = (==) `on` fst

    k :: [(a,(o,Fraction))] -> (a,Spread o)
    k ls = (fst.head $ ls,map snd ls)

    f :: (Eq a,Valence a,Ord a) => Info o a -> [(a,Spread o)]
    f = map (l.k) . groupBy h . sortBy (compare `on` fst) . concatMap g .  fromInfo

val :: (Set o,Valence a) => Info o a -> Val o (OneTuple a)
val = mkOneTuple . valuation

agg  :: Ord a => ([Double] -> Double) -> Val o a -> Rec o
agg f = Rec . M.map (f . M.elems . unRec) . unVal 

total :: Ord a => Val o a -> Rec o
total = agg (\x -> 100*sum x)

average :: Ord a => Val o a -> Rec o
average = agg (\xs->sum xs/fromIntegral (length xs))


-- val' :: (Set o,Set a,Valence a) => (a -> Spread o)  -> Val o (OneTuple a)
-- val' = (val.gather)

addAllAttrVal :: (Ord a,Ord o) => (a -> Spread o) -> [a] -> Info o a -> Val o a
addAllAttrVal f cs bs = infoToVal $ foldl (\b c -> addAttrVal c (f c) b) bs cs

addAttrVal :: (Ord a,Ord o) => a -> Spread o -> Info o a -> Info o a
addAttrVal c as bs = mkInfo [(b,f c av bv) | (a,av) <- as,(b,bv) <- bs',a == b]
    where f x xv ys = Rec $ M.insert x xv (unRec ys)
          bs' = fromInfo bs

normalize :: Valence a => a -> Spread o -> Spread o
normalize c as = let vs = [v | (_,v) <- as]
                     s = sum vs
                     s' = sum.map (\x -> 1/x) $ vs
                 in [(a,if valence c then (v/s) else ((1/v)/s')) | (a,v) <- as]


crtVal :: (Ord b,Ord o) => [(o,(b,Double))] -> Val o b
crtVal = mkVal.map f.groupBy h.sortBy (compare `on` fst)
  where
    f ls = (fst.head $ ls, mkRec . map snd $ ls)
    h :: Eq a => (a,b) -> (a,c) -> Bool
    h = \x y -> fst x == fst y


mkOneTuple :: (Ord o,Ord a) => Val o a -> Val o (OneTuple a)
mkOneTuple = mkVal . map (\(o,a) -> (o,f a)).fromVal
  where
    f = mkRec . map (\(b,n) -> (OneTuple b,n)) . fromRec

class (Projector a b,Ord d,Ord o,Set b,Set c,Valence c) => ExtendVal o a b c d | a b c -> d where
  mkTuple :: o -> (a,b,c) -> Double -> (o,(d,Double))

  extend :: Val o a -> (c -> Spread b) -> Val o d
  extend as f = extendBy as (gather f)

  extendBy :: Val o a -> Info b c -> Val o d
  extendBy as bs = crtVal [mkTuple o (aa,b,cc) (av*cv) | (o,a) <- fromVal as, (aa,av) <- fromRec a,
                           (b,c) <- (fromVal.valuation) bs, (cc,cv) <- fromRec c, proj aa==b]

instance (Set a,Set b,Valence b,Ord o) => ExtendVal o (OneTuple a) a b (a,b) where
  mkTuple o (a,_,b) n = (o,((only a,b),n))

instance (Set b,Set c,Valence c,Ord o,Ord a) => ExtendVal o (a,b) b c (a,b,c) where
  mkTuple o ((a,b),_,c) n = (o,((a,b,c),n))

instance (Set c,Set d,Valence d,Ord o,Ord a,Ord b) => ExtendVal o (a,b,c) c d (a,b,c,d) where
  mkTuple o ((a,b,c),_,d) n = (o,((a,b,c,d),n))

type Priority o = [(o,Fraction)]

priority :: Val o a -> Priority o
priority = map (\(o,a) -> (o,f a)).fromVal
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
