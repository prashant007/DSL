{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module Object where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List
import Text.Printf

import Attribute


data Obj o a = Obj {unObj :: M.Map o (Attr a)}

mkObj :: Ord o => [(o,Attr a)] -> Obj o a
mkObj = Obj. M.fromList

fromObj :: Obj o a -> [(o,Attr a)]
fromObj = M.toList .unObj


instance (Show o,Show a) => Show (Obj o a) where
  show ts = let ts' = map (\(x,y) -> show x ++ " ->\n" ++ show y) (fromObj ts)
            in "{" ++ intercalate ",\n " ts' ++ "}\n"


-- objects :: (Ord a,Ord o) => [o] -> Obj o a
-- objects os = mkObj [(o,noAttributes) | o <- os]

objects :: (Set o,Ord a) => Obj o a
objects = mkObj [(o,noAttributes) | o <- members]

addAttribute :: (Ord o,Ord a) => a -> Spread o -> Obj o a -> Obj o a
addAttribute c as bs = mkObj [(b,f c av bv) | (a,av) <- as,(b,bv) <- fromObj bs,a == b]
    where f x xv ys = Attr $ M.insert x xv (unAttr ys)

-- gather :: (Ord a,Ord o) => (a -> Spread o) -> [a] ->  [o] -> Obj o a
-- gather f as os = foldl (\o a -> addAttribute a (f a) o) (objects os) as
--
-- gather :: (Set o,Ord a) => (a -> Spread o) -> [a] -> Obj o a
-- gather f as = foldl (\o a -> addAttribute a (f a) o) objects as

gather :: (Set o,Set a) => (a -> Spread o) -> Obj o a
gather f = foldl (\o a -> addAttribute a (f a) o) objects members

addAlternative :: (Ord o,Set a) => o -> (a -> Double) -> Obj o a -> Obj o a
addAlternative o f vs = Obj $ M.insert o (mkAttr ls) (unObj vs)
    where ls = map (\x -> (x,f x)) members


-- a function for removing an attribute
delAttribute :: (Ord o,Ord a) => Obj o a -> a -> Obj o a
delAttribute os a = mkObj [(o,f a ov) | (o,ov) <- fromObj os]
        where f x xs = Attr $ M.delete x (unAttr xs)

-- a function for modifying a particular attribute value for a specific object.
-- modAttribute :: (Ord a,Ord o) => Obj o a -> a -> Spread o -> Obj o a
-- modAttribute os a = addAttribute (delAttribute os a) a

modAttribute :: (Ord a,Ord o) => Obj o a -> a -> o -> Double ->  Obj o a
modAttribute os a o' v = mkObj [if o == o' then f a o v  ov else p | p@(o,ov) <- fromObj os]
        where f a o v ov = (o,Attr $ M.insert a v (M.delete a (unAttr ov)))

-- a function for removing a dimension
delDim :: (Ord a,Ord o) => Obj o a -> [a] -> Obj o a
delDim = foldl delAttribute
