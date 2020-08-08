{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}

module MDS where

import Data.Function
import qualified Data.Map as M 
import Text.Printf
import Data.List 
import Data.Maybe(fromJust)

import Object 
import Valuation 
import Attribute 

 -- ================= MDS EXPLANATIONS ON ANNOGTATED VALUES =======================

type ValDiff a = Attr a 
type Barrier a = Attr a 
type Support a = Attr a 
type MDS a = Attr a
type Dom a = Attr a 
type Explain b = (ValDiff b,Support b,Barrier b,[Dom b],[MDS b])

explain :: Ord a => ValDiff a -> Explain a  
explain v = (v,mkAttr support,mkAttr barrier, map mkAttr ls,
             map mkAttr $ reverse $ sortBy (compare `on` f) ls') 
  where
    d = map (\(x,y) -> (x,y)) (fromAttr v)  
    (support,barrier) = partition (\(x,y) -> y>0) d 
    f = abs.sum.map snd  
    btotal = f barrier
    doms = [d | d <- subsequences support, f d > btotal]  
    ls = sortBy (compare `on` length) doms
    ls'= takeWhile (\p -> length p == (length.head) ls) ls


select :: Eq o => o -> Obj o a -> Attr a 
select o = fromJust.lookup o.fromObj

-- ================== GENERALIZE FUNCTIONS ================================
-- ========================================================================

class Generalize a b | a -> b where
      generalize :: Attr a -> Explain b

instance Ord a => Generalize (a,b) a where
      generalize = explain.mkAttr.sumOutSnd.fromAttr 

instance Ord b => Generalize (a,b) b where
      generalize = explain.mkAttr.sumOutFst.fromAttr 
              
instance (Ord a,Ord b) => Generalize (a,b,c) a where
  generalize = explain.mkAttr.justFst.fromAttr 

instance (Ord b,Ord c) => Generalize (a,b,c) b where
  generalize = explain.mkAttr.justSnd.fromAttr

instance (Ord c,Ord a) => Generalize (a,b,c) c where
  generalize = explain.mkAttr.justTrd.fromAttr
                

-- ========================= PRITNTING EXPLANATIONS =====================
-- ======================================================================

pdom :: Show b => Explain b -> IO ()
pdom (x,y,z,w,_) = do 
  ph (x,y,z)
  putStrLn "\nDominators:"
  mapM_ pd w  

pmds :: Show b => Explain b -> IO ()
pmds (x,y,z,_,w) = do 
  ph (x,y,z)
  putStrLn "\nMDS:"
  mapM_ pd w  

-- ========================= HELPER FUNCTIONS============================
-- ======================================================================

pd :: Show a => a -> IO ()
pd = putStrLn.show 

ph :: Show b => (ValDiff b,Support b,Barrier b) -> IO ()
ph (a,b,c) = do 
      putStrLn "\nValue Difference:"
      putStrLn $ show a
      putStrLn "\nSupport:"
      pd b
      putStrLn "\nBarrier:"
      pd c


sumOutSnd :: (Num n,Ord a) => [((a,b),n)] -> [(a,n)]
sumOutSnd = map h.groupBy g.sortBy (compare `on` f)
    where
        h xs = ((f.head) xs,(sum.map snd) xs)
        g x y = f x == f y
        f = fst.fst

sumOutFst :: (Num n,Ord b) => [((a,b),n)] -> [(b,n)]
sumOutFst = sumOutSnd.reorderKey

reorderKey :: [((a,b),n)] -> [((b,a),n)]
reorderKey = map (\((x,y),z) -> ((y,x),z))

justFst :: (Num n,Ord a,Ord b) => [((a,b,c),n)] -> [(a,n)]
justFst ls = let f  = \((x,y,z),n) -> (((x,y),z),n)
                 ls'= sumOutSnd $ map f ls 
             in sumOutSnd ls'

justSnd :: (Num n,Ord b,Ord c) => [((a,b,c),n)] -> [(b,n)]
justSnd ls = let f = \((x,y,z),n) -> ((y,z,x),n) 
             in justFst (map f ls)  


justTrd :: (Num n,Ord c,Ord a) => [((a,b,c),n)] -> [(c,n)]
justTrd ls = let f = \((x,y,z),n) -> ((z,x,y),n) 
             in justFst (map f ls)  
