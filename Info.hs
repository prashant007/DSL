{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module Info where

import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe (fromJust)
import Text.Printf

import Record


data Info o a = Info {unInfo :: M.Map o (Rec a)}

mkInfo :: Ord o => [(o,Rec a)] -> Info o a
mkInfo = Info . M.fromList

info :: (Ord o,Ord a) => [(o,[(a,Double)])] -> Info o a
info oas = mkInfo [(o,mkRec as) | (o,as) <- oas]

-- newInfo :: Ord o => Info o a
-- newInfo = Info $ M.empty

fromInfo :: Info o a -> [(o,Rec a)]
fromInfo = M.toList . unInfo


-- agg :: (Rec a -> b) -> Info o a -> Rec o b
-- agg f = Info . M.map f . unInfo


instance (Show o,Show a) => Show (Info o a) where
  show ts = let ts' = map (\(x,y) -> show x ++ " ->\n" ++ show y) (fromInfo ts) 
            in "{" ++ intercalate ",\n " ts' ++ "}\n"


objects :: (Set o,Ord a) => Info o a
objects = mkInfo [(o,emptyRec) | o <- members]

addAttribute :: (Ord o,Ord a) => a -> Spread o -> Info o a -> Info o a
addAttribute c as bs = mkInfo [(b,f c av bv) | (a,av) <- as,(b,bv) <- fromInfo bs,a == b]
    where f x xv ys = Rec $ M.insert x xv (unRec ys)

gather :: (Set o,Set a) => (a -> Spread o) -> Info o a
gather f = foldl (\o a -> addAttribute a (f a) o) objects members

addAlternative :: (Ord o,Set a) => o -> (a -> Double) -> Info o a -> Info o a
addAlternative o f vs = Info $ M.insert o (mkRec ls) (unInfo vs)
    where ls = map (\x -> (x,f x)) members


-- a function for removing an attribute
delAttribute :: (Ord o,Ord a) => Info o a -> a -> Info o a
delAttribute os a = mkInfo [(o,f a ov) | (o,ov) <- fromInfo os]
        where f x xs = Rec $ M.delete x (unRec xs)

-- a function for modifying a particular attribute value for a specific object.
-- modAttribute :: (Ord a,Ord o) => Info o a -> a -> Spread o -> Info o a
-- modAttribute os a = addAttribute (delAttribute os a) a

modAttribute :: (Ord a,Ord o) => Info o a -> a -> o -> Double ->  Info o a
modAttribute os a o' v = mkInfo [if o == o' then f a o v  ov else p | p@(o,ov) <- fromInfo os]
        where f a o v ov = (o,Rec $ M.insert a v (M.delete a (unRec ov)))

-- a function for removing a dimension
delDim :: (Ord a,Ord o) => Info o a -> [a] -> Info o a
delDim = foldl delAttribute
