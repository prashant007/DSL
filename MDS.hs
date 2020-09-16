{-# LANGUAGE  MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,DataKinds #-}

module MDS where

import Data.Function
import qualified Data.Map as M
import Data.Maybe
import Text.Printf
import Data.List

import Record
import Info
import Valuation

 -- ================= MDS EXPLANATIONS ON ANNOGTATED VALUES =======================

type ValDiff a = Rec a
type Barrier a = Rec a
type Support a = Rec a
type MDS a = Rec a
type Dom a = Rec a
type Explain b = (ValDiff b,Support b,Barrier b,[Dom b],[MDS b])


explain :: Ord a => ValDiff a -> Explain a
explain v = (v,mkRec support,mkRec barrier, map mkRec ls,
             map mkRec $ reverse $ sortBy (Prelude.compare `on` f) ls')
  where
    d = map (\(x,y) -> (x,y)) (fromRec v)
    (support,barrier) = partition (\(x,y) -> y>0) d
    f = abs.sum.map snd
    btotal = f barrier
    doms = [d | d <- subsequences support, f d > btotal]
    ls = sortBy (Prelude.compare `on` length) doms
    ls'= takeWhile (\p -> length p == (length.head) ls) ls


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


<<<<<<< HEAD

select :: Eq o => o -> Info o a -> Rec a 
select o = fromJust . lookup o . fromInfo

(!) :: Eq o => Val o a -> o -> Rec a  
(!) = flip select 

compare :: (Ord a,Eq o) => Info o a -> o -> o -> Rec a  
=======
class (Eq o,Num b) => Select o a b | a -> b where
  toLookupList :: a -> [(o,b)]

select :: Select o a b => o -> a -> b
select o = fromJust . lookup o . toLookupList

(!) :: Select o a b => a -> o -> b
(!) = flip select

compare :: Select o a b => a -> o -> o -> b 
>>>>>>> 1e75913aea32cf939ce0c8599b5f7bcb3b8b094f
compare i o1 o2 = i!o1 - i!o2



<<<<<<< HEAD
-- instance Select o (Info o a) (Rec a) where
--   func = 
=======
instance (Eq o,Ord a) => Select o (Info o a) (Rec a) where
  toLookupList = fromInfo

-- instance Select o (Info o a) (Norm a) where
--   func =
>>>>>>> 1e75913aea32cf939ce0c8599b5f7bcb3b8b094f

-- select :: Eq o => o -> Val o a -> Rec a
-- select o = fromJust . lookup o . fromVal

<<<<<<< HEAD
-- (!) :: Eq o => Val o a -> o -> Rec a
-- (!) = flip select 
=======
-- (!) :: Eq o => Val o a -> o -> Norm a
-- (!) = flip select
>>>>>>> 1e75913aea32cf939ce0c8599b5f7bcb3b8b094f
