{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}


module EvalValuations where

import StabilityHelper 
import Valuation 
import Dimension
import Record
import Info

import qualified Data.Map as M


-- ====================== GENERATING VALUATIONS ==========================
-- =======================================================================

type ValStr2 = Val String (String,String) 
type ValStr3 = Val String (String,String,String)
type ValStr4 = Val String (String,String,String,String)
type ValStr5 = Val String (String,String,String,String,String)



-- this is for the case when we already have valuations 
extendByE :: (Ord o,Ord b,Ord d,Expand a c d,Covers a b) => Val o a -> Val b c -> Val o d
extendByE as bs = listToInfo
                     [((o,expand aa cc),(av*cv)) |
                      (o,a) <- fromInfo as,(aa,av) <- fromRec a,
                      (b,c) <- fromInfo bs,(cc,cv) <- fromRec c,
                      project aa == b]

-- a is the tuple of info values. o is the type of the argument.
-- b is constituent of Val o b, the resultant type of the valuation. 

class FinVal a o b | a -> o b where
    finval :: a -> Val o b 

instance (Ord a,Ord b,Ord o) => FinVal (Info2 o a b) o (a,b) where
    finval (x,y) = (mkOneTuple x `extendByE` y)

instance (Ord o,Ord a,Ord b,Ord c) => FinVal (Info3 o a b c) o (a,b,c) where
    finval (x,y,z) = (mkOneTuple x `extendByE` y `extendByE` z)

instance (Ord o,Ord a,Ord b,Ord c,Ord d) => FinVal (Info4 o a b c d) o (a,b,c,d) where
    finval (x,y,z,w) = (mkOneTuple x `extendByE` y `extendByE` z `extendByE` w)

instance (Ord o,Ord a,Ord b,Ord c,Ord d,Ord e) => FinVal (Info5 o a b c d e) o (a,b,c,d,e) where
    finval (x,y,z,v,w) = (mkOneTuple x `extendByE` y `extendByE` z `extendByE` v `extendByE` w)



composeVals2 :: [Val String String] -> ValStr2 
composeVals2 vs = finval (vs!!0,vs!!1)

composeVals3 :: [Val String String] -> ValStr3
composeVals3 vs = finval (vs!!0,vs!!1,vs!!2)

composeVals4 :: [Val String String] -> ValStr4 
composeVals4 vs = finval (vs!!0,vs!!1,vs!!2,vs!!3)

composeVals5 :: [Val String String] -> ValStr5 
composeVals5 vs = finval (vs!!0,vs!!1,vs!!2,vs!!3,vs!!4)

focusOnLev2 :: (FocusOnLevel2 a,Ord a) => Val String a -> Val String String 
focusOnLev2 = mapInfo focusRec2

focusOnLev3 :: (FocusOnLevel3 a,Ord a) => Val String a -> Val String String 
focusOnLev3 = mapInfo focusRec3

focusOnLev4 :: (FocusOnLevel4 a,Ord a) => Val String a -> Val String String 
focusOnLev4 = mapInfo focusRec4

focusOnLev5 :: (FocusOnLevel5 a,Ord a) => Val String a -> Val String String 
focusOnLev5 = mapInfo focusRec5


class FocusOnLevel2 a where
    noOfDims  :: Val String a -> Int  
    focusRec2 :: Rec a -> Rec String 

instance FocusOnLevel2 (String,String) where
    noOfDims _ = 2
    focusRec2 = Rec . M.map sumRec . groupRecBy fst

instance FocusOnLevel2 (String,String,String) where
    noOfDims _ = 3 
    focusRec2 = Rec . M.map sumRec . groupRecBy (\(x,y,z) -> x)

instance FocusOnLevel2 (String,String,String,String) where
    noOfDims _ = 4
    focusRec2 = Rec . M.map sumRec . groupRecBy (\(x,y,z,w) -> x)

instance FocusOnLevel2 (String,String,String,String,String) where
    noOfDims _ = 5
    focusRec2 = Rec . M.map sumRec . groupRecBy (\(x,y,z,w,a) -> x)


class FocusOnLevel3 a where
    focusRec3 :: Rec a -> Rec String 

instance FocusOnLevel3 (String,String) where
    focusRec3 = Rec . M.map sumRec . groupRecBy snd

instance FocusOnLevel3 (String,String,String) where
    focusRec3 = Rec . M.map sumRec . groupRecBy (\(x,y,z) -> y)

instance FocusOnLevel3 (String,String,String,String) where
    focusRec3 = Rec . M.map sumRec . groupRecBy (\(x,y,z,w) -> y)

instance FocusOnLevel3 (String,String,String,String,String) where
    focusRec3 = Rec . M.map sumRec . groupRecBy (\(x,y,z,w,a) -> y)



class FocusOnLevel4 a where
    focusRec4 :: Rec a -> Rec String 

instance FocusOnLevel4 (String,String,String) where
    focusRec4 = Rec . M.map sumRec . groupRecBy (\(x,y,z) -> z)

instance FocusOnLevel4 (String,String,String,String) where
    focusRec4 = Rec . M.map sumRec . groupRecBy (\(x,y,z,w) -> z)

instance FocusOnLevel4 (String,String,String,String,String) where
    focusRec4 = Rec . M.map sumRec . groupRecBy (\(x,y,z,w,a) -> z)


class FocusOnLevel5 a where
    focusRec5 :: Rec a -> Rec String 

instance FocusOnLevel5 (String,String,String,String) where
    focusRec5 = Rec . M.map sumRec . groupRecBy (\(x,y,z,w) -> w)

instance FocusOnLevel5 (String,String,String,String,String) where
    focusRec5 = Rec . M.map sumRec . groupRecBy (\(x,y,z,w,a) -> w)