{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module Classes where

import Data.Tuple.OneTuple (only,OneTuple(..))

class Covers a b | a -> b where
  project :: a -> b

instance Covers (OneTuple a) a where project = only
instance Covers (a,b) a        where project (a,b) = a
instance Covers (a,b) b        where project (a,b) = b
instance Covers (a,b,c) a      where project (a,b,c) = a
instance Covers (a,b,c) b      where project (a,b,c) = b
instance Covers (a,b,c) c      where project (a,b,c) = c
instance Covers (a,b,c,d) a    where project (a,b,c,d) = a
instance Covers (a,b,c,d) b    where project (a,b,c,d) = b
instance Covers (a,b,c,d) c    where project (a,b,c,d) = c
instance Covers (a,b,c,d) d    where project (a,b,c,d) = d

class Shrink a b | a -> b where
  shrink :: a -> b

instance Shrink (OneTuple a) ()   where shrink _ = ()
instance Shrink (a,b) a           where shrink = fst
instance Shrink (a,b) b           where shrink = snd
instance Shrink (a,b,c) (a,b)     where shrink (a,b,c) = (a,b)
instance Shrink (a,b,c) (b,c)     where shrink (a,b,c) = (b,c)
instance Shrink (a,b,c) (a,c)     where shrink (a,b,c) = (a,c)
instance Shrink (a,b,c,d) (a,b,c) where shrink (a,b,c,d) = (a,b,c)
instance Shrink (a,b,c,d) (b,c,d) where shrink (a,b,c,d) = (b,c,d)
instance Shrink (a,b,c,d) (a,c,d) where shrink (a,b,c,d) = (a,c,d)
instance Shrink (a,b,c,d) (a,b,d) where shrink (a,b,c,d) = (a,b,d)


class Append a c d | a c -> d where
  append :: a -> c -> d

instance Append (OneTuple a) b (a,b)    where append a b = (only a,b)
instance Append (a,b)     c (a,b,c)     where append (a,b) c = (a,b,c)
instance Append (a,b,c)   d (a,b,c,d)   where append (a,b,c) d = (a,b,c,d)
instance Append (a,b,c,d) e (a,b,c,d,e) where append (a,b,c,d) e = (a,b,c,d,e)
