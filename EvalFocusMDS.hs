import Evaluation
import EvalValuations
import StabilityHelper 
import Valuation 
import Record
import Info
import Dimension
import MDS 
import GenAHPVals


-- focusFact2 :: Ord a => Val Attr a -> (Attr,Attr) -> AttrCount 
-- focusFact2 v (x,y) = fromIntegral mlen
--     where mlen = length . fromRec . mds1 $ diff v x y


-- focusFact2 :: Ord a => Val Attr a -> (Attr,Attr) -> ((Attr,Attr),Rec a)  
-- focusFact2 v (x,y) = ((x,y),mds1 $ diff v x y)

-- avgL :: [Double] -> Double 
-- avgL xs = sum xs/ fromIntegral (length xs)

-- levelFocus4 :: (FocusOnLevel3 a,FocusOnLevel2 a,Ord a) => AttrCount -> Val String a -> [(Attr,Attr)] -> FocusFactor 
-- levelFocus4 n v as = 
--     let v2 = focusOnLev2 v 
--         v3 = focusOnLev3 v 
--         ffLev4 a = fromIntegral (focusFact2 v2 a + focusFact2 v3 a)/fromIntegral n  
--     in avgL $ map ffLev4 as   



testFocus :: Ratio -> Levels -> IO FocusVal 
testFocus r l = do 
      ds <- genAHPDims l (2,10) 
      vs <- genFinVals ds 
      let d1 = ds !! 0
          n  = mulL.tail $ ds
      case length vs of 
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
              False -> do 
                testLevel r l 