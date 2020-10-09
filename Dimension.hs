{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module Dimension where

import Data.Tuple.OneTuple (only,OneTuple(..))

class Covers t a | t -> a where
  project :: t -> a

instance Covers (OneTuple a) a where project = only
instance Covers (a,b) a        where project = fst
instance Covers (a,b) b        where project = snd
instance Covers (a,b,c) a      where project (a,b,c) = a
instance Covers (a,b,c) b      where project (a,b,c) = b
instance Covers (a,b,c) c      where project (a,b,c) = c
instance Covers (a,b,c,d) a    where project (a,b,c,d) = a
instance Covers (a,b,c,d) b    where project (a,b,c,d) = b
instance Covers (a,b,c,d) c    where project (a,b,c,d) = c
instance Covers (a,b,c,d) d    where project (a,b,c,d) = d


class Shrink t t' | t -> t' where
  shrink :: t -> t'

instance Shrink (OneTuple a) ()   where shrink _ = ()
instance Shrink (a,b) b           where shrink = snd
instance Shrink (a,b) a           where shrink = fst
instance Shrink (a,b,c) (b,c)     where shrink (a,b,c) = (b,c)
instance Shrink (a,b,c) (a,c)     where shrink (a,b,c) = (a,c)
instance Shrink (a,b,c) (a,b)     where shrink (a,b,c) = (a,b)
instance Shrink (a,b,c,d) (b,c,d) where shrink (a,b,c,d) = (b,c,d)
instance Shrink (a,b,c,d) (a,c,d) where shrink (a,b,c,d) = (a,c,d)
instance Shrink (a,b,c,d) (a,b,d) where shrink (a,b,c,d) = (a,b,d)
instance Shrink (a,b,c,d) (a,b,c) where shrink (a,b,c,d) = (a,b,c)


class (Covers t a,Shrink t t') => Split t a t' where {}

instance Split (OneTuple a) a () where
instance Split (a,b)     a b
instance Split (a,b)     b a
instance Split (a,b,c)   a (b,c)
instance Split (a,b,c)   b (a,c)
instance Split (a,b,c)   c (a,b)
instance Split (a,b,c,d) a (b,c,d)
instance Split (a,b,c,d) b (a,c,d)
instance Split (a,b,c,d) c (a,b,d)
instance Split (a,b,c,d) d (a,b,c)


class Expand a c d | a c -> d where
  expand :: a -> c -> d

instance Expand (OneTuple a) b (a,b)    where expand a b = (only a,b)
instance Expand (a,b)     c (a,b,c)     where expand (a,b) c = (a,b,c)
instance Expand (a,b,c)   d (a,b,c,d)   where expand (a,b,c) d = (a,b,c,d)
instance Expand (a,b,c,d) e (a,b,c,d,e) where expand (a,b,c,d) e = (a,b,c,d,e)
