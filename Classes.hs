{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module Classes where

import Data.Tuple.OneTuple (only,OneTuple(..))

class SubDim  a b | a -> b where
  proj :: a -> b 

instance SubDim  (OneTuple a) a  where 
  proj = only

instance SubDim  (a,b) a where
  proj (a,b) = a

instance SubDim  (a,b) b where
  proj (a,b) = b 

instance SubDim  (a,b,c) a where
  proj (a,b,c) = a

instance SubDim  (a,b,c) b where
  proj (a,b,c) = b 

instance SubDim  (a,b,c) c where
  proj (a,b,c) = c

instance SubDim  (a,b,c,d) a where
  proj (a,b,c,d) = a 

instance SubDim  (a,b,c,d) b where
  proj (a,b,c,d) = b 

instance SubDim  (a,b,c,d) c where
  proj (a,b,c,d) = c 

instance SubDim  (a,b,c,d) d where
  proj (a,b,c,d) = d 


class Reduce a b | a -> b where
  reducedTup :: a -> b 

instance Reduce (OneTuple a) ()  where 
  reducedTup _ = ()

instance Reduce (a,b) a  where 
  reducedTup (a,b) = a 

instance Reduce (a,b) b  where 
  reducedTup (a,b) = b 

instance Reduce (a,b,c) (a,b) where 
  reducedTup (a,b,c) = (a,b)

instance Reduce (a,b,c) (b,c) where 
  reducedTup (a,b,c) = (b,c)

instance Reduce (a,b,c) (a,c) where 
  reducedTup (a,b,c) = (a,c)

instance Reduce (a,b,c,d) (a,b,c) where 
  reducedTup (a,b,c,d) = (a,b,c)

instance Reduce (a,b,c,d) (b,c,d) where 
  reducedTup (a,b,c,d) = (b,c,d)

instance Reduce (a,b,c,d) (a,c,d) where 
  reducedTup (a,b,c,d) = (a,c,d)

instance Reduce (a,b,c,d) (a,b,d) where 
  reducedTup (a,b,c,d) = (a,b,d)  

class Tuple a c d | a c -> d where
  mkTuple :: a -> c -> d 

instance Tuple (OneTuple a) b (a,b) where
  mkTuple a b = (only a,b)

instance Tuple (a,b) c (a,b,c) where
  mkTuple (a,b) c = (a,b,c)

instance Tuple (a,b,c) d (a,b,c,d) where
  mkTuple (a,b,c) d = (a,b,c,d)

instance Tuple (a,b,c,d) e (a,b,c,d,e) where
  mkTuple (a,b,c,d) e = (a,b,c,d,e)