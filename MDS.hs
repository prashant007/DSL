{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies #-}
module MDS where

import qualified Data.Map.Strict as M
import Data.Function
import Data.List
import Text.Printf

import Record
import Info
import Valuation


-- Analysis of Decisions:
--  (support, barrier, dominators, mds)
--
type Analysis a = (Rec a,Rec a,[Rec a],[Rec a])


-- Summarized set
--
data SumSet a = SumSet [a] Double

instance Show a => Show (SumSet a) where
  show (SumSet xs d) = showSet (map show xs) ++ " : " ++ printf ("%.2f") d


-- Dominance relation
--
data Dominance a = Dominance (SumSet a) (SumSet a)

instance Show a => Show (Dominance a) where
  show (Dominance x y) = show x ++ " > |" ++ show y ++"|"



analyze :: Ord a => Rec a -> Analysis a
analyze v = (mkRec support,mkRec barrier, map mkRec sdoms,map mkRec smdss)
  where
    (support,barrier) = partition ((>0) . snd) (fromRec v)
    absSum = abs . sum . map snd
    doms   = [d | d <- subsequences support, absSum d > absSum barrier]
    sdoms  = sortBy (compare `on` length) doms
    mdss   = takeWhile (\p -> length p == (length.head) sdoms) sdoms
    smdss  = reverse $ sortBy (compare `on` absSum) mdss

dominators :: Ord a => Rec a -> [Rec a]
dominators r = ds where (_,_,ds,_) = analyze r

mds :: Ord a => Rec a -> [Rec a]
mds r = ds where (_,_,_,ds) = analyze r

explain :: Ord a => Rec a -> Dominance a
explain r = Dominance (total d) (total b)
            where (_,b,_,d:_) = analyze r


instance Aggregate (Rec a) (SumSet a) where
  agg f r = SumSet dom (f rng)
             where m = unRec r
                   dom = M.keys m
                   rng = M.elems m
