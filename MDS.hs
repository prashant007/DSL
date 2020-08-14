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

