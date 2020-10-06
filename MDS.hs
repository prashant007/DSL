module MDS where

import qualified Data.Map.Strict as M
import Data.Function
import Data.List
import Text.Printf

import Record
import Info
import Valuation


-- Explanation Structure:
--  (support, barrier, dominators, mds)
--
type Explain a = (Rec a,Rec a,[Rec a],[Rec a])

-- Summarized set
--
data SumSet a = SumSet {unSumSet :: ([a],Double)}

instance Show a => Show (SumSet a) where
  show (SumSet (xs,d))= showSet (map show xs) ++ " : " ++ printf ("%.2f") d


explain :: Ord a => Rec a -> Explain a
explain v = (mkRec support,mkRec barrier, map mkRec sdoms,map mkRec smdss)
  where
    (support,barrier) = partition ((>0) . snd) (fromRec v)
    absSum = abs . sum . map snd
    doms   = [d | d <- subsequences support, absSum d > absSum barrier]
    sdoms  = sortBy (compare `on` length) doms
    mdss   = takeWhile (\p -> length p == (length.head) sdoms) sdoms
    smdss  = reverse $ sortBy (compare `on` absSum) mdss

dominators :: Ord a => Rec a -> [Rec a]
dominators r = ds where (_,_,ds,_) = explain r

mds :: Ord a => Rec a -> [Rec a]
mds r = ds where (_,_,_,ds) = explain r

ragg  :: Ord a => ([Double] -> Double) -> Rec a -> SumSet a
ragg f r = SumSet (dom,f rng)
           where m = unRec r
                 dom = M.keys m
                 rng = M.elems m

rsum :: Ord a => Rec a -> SumSet a
rsum = ragg sum
