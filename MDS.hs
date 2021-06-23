{-# LANGUAGE  MultiParamTypeClasses #-}

module MDS where

import qualified Data.Map.Strict as M
import Data.Function
import Data.List
import Text.Printf

import Record
import Info
import Valuation
import Dimension


-- Analysis of Decisions:
--  (support, barrier, dominators, mds)
--
type Analysis a = (Rec a,Rec a,[Rec a],[Rec a])


-- Summarized set
--
data SumSet a = SumSet [a] Double deriving Eq 

instance Eq a => Ord (SumSet a) where
   SumSet _ x <= SumSet _ y = x <= y 


instance Show a => Show (SumSet a) where
  show (SumSet xs d) = showSet (map show xs) ++ " : " ++ printf ("%.2f") d


-- Dominance relation
--
data Dominance a = Dominance (SumSet a) (SumSet a)

instance Show a => Show (Dominance a) where
  show (Dominance x y) = show x ++ " > |" ++ show y ++ "|"


-- Explanation
--
data Explanation o a = Explanation o o (Dominance a)

instance (Show o,Show a) => Show (Explanation o a) where
  show (Explanation w r d) =
       show w ++ " is the best option; it is better than " ++ show r
                             ++ " because\n" ++ show d


analyze :: Ord a => Rec a -> Analysis a
analyze v = (mkRec support,mkRec barrier, map mkRec sdoms,map mkRec smdss)
  where
    (support,barrier) = partition ((>0) . snd) (fromRec v)
    absSum = abs . sum . map snd
    doms   = [d | d <- subsequences support, absSum d > absSum barrier]
    sdoms  = sortBy (compare `on` length) doms
    mdss   = takeWhile (\p -> length p == (length.head) sdoms) sdoms
    smdss  = reverse $ sortBy (compare `on` absSum) mdss

barrier :: Ord a => Rec a -> Rec a
barrier r = b where (_,b,_,_) = analyze r

dominators :: Ord a => Rec a -> [Rec a]
dominators r = ds where (_,_,ds,_) = analyze r

mds :: Ord a => Rec a -> [Rec a]
mds r = ds where (_,_,_,ds) = analyze r

dominance :: Ord a => Rec a -> Dominance a
dominance r = Dominance (total d) (total b)
              where (_,b,_,d:_) = analyze r

-- explain :: (Ord o,Ord a,Ord b,Shrink a b) => Val o a -> Explanation o b
-- explain v = Explanation win rup (Dominance (total d) (total b))
--             where (win,rup)   = (winner v, runnerUp v)
--                   (_,b,_,d:_) = analyze $ diff (shrinkVal v) win rup

explain :: (Ord o,Ord a) => Val o a -> Explanation o a
explain v = Explanation win rup (Dominance (total d) (total b))
            where (win,rup)   = (winner v, runnerUp v)
                  (_,b,_,d:_) = analyze $ diff v win rup

instance Aggregate (Rec a) (SumSet a) where
  agg f r = SumSet dom (f rng)
             where m = unRec r
                   dom = M.keys m
                   rng = M.elems m
