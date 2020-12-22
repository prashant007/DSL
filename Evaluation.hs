module Evaluation where

import System.Random.MWC.Probability
import Control.Monad.State
import Data.List hiding (transpose)
import System.Random hiding (uniformR)
import Control.Monad.Random as R 
import Data.Function (on)

import EvalValuations
import StabilityHelper 
import Valuation 
import Record
import Info
import Dimension
import MDS 
import GenAHPVals


-- ==================================================================================== 
type SampleCount = Int 
type FocusFactor = Double
type ReductionFactor = Double 

type FocusVal = ((Levels,AttrCount),(SampleCount,FocusFactor))
type ReductionVal = ((Levels,AttrCount),(SampleCount,ReductionFactor))

type Levels = Int 


-- The attribute count at the botton most level can be
-- used to know what alternatives were generated 
genPairs :: Eq a => Ratio -> AttrCount -> Val Attr a -> [(Attr,Attr)]
genPairs r c v = [(a,b) | a <- as, b <- as, ratio (totV a) (totV b)]
  where
    as = map (\x -> "A1" ++ show x) [1..c]
    ratio x y  = x/y > 1 && x/y <= r
    v' = total v 
    totV x = lookupRec x v' 


-- Second argument represents the total number of arguments in a valuation for an alternative
focusFactL1 :: Ord a => Levels -> AttrCount -> Val Attr a -> [(Attr,Attr)] -> FocusVal                      
focusFactL1 l n v ls = ((l,n),(length ls,sum . map (focusFact1 n v) $ ls)) 

focusFact1 :: Ord a => AttrCount -> Val Attr a -> (Attr,Attr) -> FocusFactor 
focusFact1 n v (x,y) = fromIntegral mlen/ fromIntegral n
    where 
          mlen = length . fromRec . mds' $ diff v x y
          mds' = if flag == 0 then (head.mds) else mds1 


flag = 1

-- ==================================================================
-- ==================================================================

type AttrCount3 = (AttrCount,AttrCount,AttrCount)
type AttrCount4 = (AttrCount,AttrCount,AttrCount,AttrCount)
type AttrCount5 = (AttrCount,AttrCount,AttrCount,AttrCount,AttrCount)
type AttrCount6 = (AttrCount,AttrCount,AttrCount,AttrCount,AttrCount,AttrCount)

genAHPDims :: Int -> (Int,Int) -> IO [Int]
genAHPDims n p = do 
  alts <- getRandomR (2,5) -- the alternatives 
  let last = [1]
  rem  <- getRandomRs p 
  let rem' = take (n-2) rem
  case mulL rem' <= 400 of
    True  -> return $ alts:rem' ++ [1]
    False -> genAHPDims n p 


genTests :: Ratio -> Levels -> SampleCount -> IO [ReductionVal]
genTests r l s = do 
    xs <- mapM (\_ -> testLevel r l) $ [1..s]
    return $ map (toReductionVal) $ getAvgFocus xs  
    where
      toReductionVal :: FocusVal -> ReductionVal 
      toReductionVal (x,(y,z)) = (x,(y,(1-z)*100))

      getAvgFocus :: [FocusVal] -> [FocusVal]
      getAvgFocus xs = map sumGroup $ groupElem xs
          where
            groupElem = groupBy ((==) `on` fst) . sort 
            sumPairs  = foldl (\(a,b) (c,d) -> (a+c,b+d)) (0,0)
            avgGrpElems = (\(x,y) -> (x, y/fromIntegral x)) . sumPairs . map snd
            sumGroup x = (fst.head $ x, avgGrpElems x) 


mulL :: [Int] -> Int 
mulL = foldl ((*)) 1 

testLevel :: Ratio -> Levels -> IO FocusVal 
testLevel r l = do 
      ds <- genAHPDims l (2,20) 
      vs <- genFinVals ds 
      let d1 = ds !! 0
          n  = mulL.tail $ ds
      case length ds of 
        3 -> checkEmpty l (d1,n) (composeVals2 vs) r
        4 -> checkEmpty l (d1,n) (composeVals3 vs) r
        5 -> checkEmpty l (d1,n) (composeVals4 vs) r
        6 -> checkEmpty l (d1,n) (composeVals5 vs) r
        x -> error $  show x
      where 
        checkEmpty :: Ord b => Levels -> (AttrCount,AttrCount) -> Val String b -> Ratio -> IO FocusVal 
        checkEmpty l (d1,n) xs r = do 
            let as = genPairs r d1 xs 
            case as /= [] of 
              True  -> return $ focusFactL1 l n xs as 
              False -> testLevel r l 

-- ==================================================================
-- ==================================================================

--minimal dominating sets
mds1 :: Ord a => Rec a -> Rec a 
mds1 r = let 
             (support,barrier) = partition ((>0) . snd) (fromRec r)
             absSum = abs . sum . map snd
             sumBarrier = absSum barrier
             sortedSupp = reverse . sortBy (compare `on` snd) $ support

             mdsHelp :: Double -> [(a,Double)] -> [(a,Double)] -> [(a,Double)]
             mdsHelp v [] ps = ps 
             mdsHelp v (a@(_,av):xs) ps 
               | vav >= 0  = mdsHelp vav xs aps
               | otherwise = aps
               where 
                 vav = v - av 
                 aps = a:ps 

         in mkRec $ mdsHelp sumBarrier sortedSupp []
