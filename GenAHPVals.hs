module GenAHPVals where

import System.Random.MWC.Probability
import Control.Monad.State
import Data.List hiding (transpose)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Random as R 
import System.Random hiding (uniformR)
import Data.Array.ST
import GHC.Arr

import Record
import Info
import Valuation 


type SampleNos = Int 
type SampSize  = Int 
type Ratio = Double


-- ========== GENERATING RANDOM NUMBERS =========================
-- ==============================================================

-- sampSize - Number of elements in one sample
-- sampleNos - Number of samples required 
dirichletDist :: Ratio -> SampSize -> SampleNos -> IO [[Double]]
dirichletDist r ss sn = do 
  gen <- createSystemRandom 
  xs  <- dirichletGen r gen ss sn 
  
  let ys = filter ratios xs 
      l  = length ys
      ratios x = let x' = sort x in last x'/head x' <= r  

  case l == sn of 
    True  -> return ys 
    False -> do 
      ys' <- dirichletDist r ss (sn-l) 
      return $ ys' ++ ys


dirichletGen :: Ratio -> GenIO -> SampSize -> SampleNos -> IO [[Double]]
dirichletGen r gen ss sn 
  | r == 1 = do 
      let xs'= take ss (repeat (1/fromIntegral ss))
          xs = take sn (repeat xs')
      return xs 
  | otherwise = nsamples r gen ss sn


nsamples :: Ratio -> Gen (PrimState IO) -> SampSize -> SampleNos -> IO [[Double]]
nsamples r g ss 0 = return []
nsamples r g ss sn = do
    xs  <- nsampsH r g ss 
    xxs <- nsamples r g ss (sn-1) 
    mkRand $ concat xs:xxs 
  where 
      mkRand = sequence . map (evalRandIO . shuffle)
      nsampsH r g ss 
          | r <= 2   = do 
                ns <- samples 1 (uniformR (0.001,100)) g
                samples 1 (symmetricDirichlet ss (head ns)) g
          | otherwise = do 
                ns <- samples ss (uniformR (0.00001,10000)) g
                samples 1 (dirichlet ns) g 


 
shuffle :: R.RandomGen g => [a] -> Rand g [a]
shuffle xs = do
    let l = length xs
    rands <- forM [0..(l-2)] $ \i -> getRandomR (i, l-1)
    let ar = runSTArray $ do
        ar <- thawSTArray $ listArray (0, l-1) xs
        forM_ (zip [0..] rands) $ \(i, j) -> do
            vi <- readSTArray ar i
            vj <- readSTArray ar j
            writeSTArray ar j vi
            writeSTArray ar i vj
        return ar
    return (elems ar)

ex1 r = do 
  xs <- dirichletDist r 5 10  
  mapM_ (putStrLn.show) xs 

-- ================= CREATING AHP FROM THE RANDOM NUMBERS GENERATED ABOVE====
-- ==========================================================================

type AttrCount = Int 
type Attr = String 

genAHPmodel :: [AttrCount] -> State Int [[Attr]]
genAHPmodel [] = return []
genAHPmodel (c:cs) = do 
    c' <- genAttrs c 
    cs'<- genAHPmodel cs 
    return $ c':cs'   
  where
      genAttrs :: AttrCount -> State Int [Attr]
      genAttrs n = do 
        l <- get 
        let l' = ['A'..'Z']!!(l-1):[] ++ show l 
            vs = map (\x -> l' ++ show x) $ [1..n]
        modify $ \a -> a+1
        return vs 

genVals :: [[Attr]] -> IO [Val String String]
genVals []  = return []
genVals [l] = return []
genVals (a1:a2:as) = do 
  nns  <- dirichletDist 20 (length a1) (length a2)
  let ts = [zipWith (\x1 n -> ((a2!!l2,x1),n)) a1 (nns !! l2) | l2 <- [0..length a2 - 1]] 
      v  = transpose . listToInfo . concat $ ts 
  vs <- genVals (a2:as)
  return $ v:vs

-- generate valuations for all the consecutive levels 
genFinVals :: [AttrCount] -> IO [Val String String]
genFinVals cs = do 
  let m = evalState (genAHPmodel cs) 1
  genVals m  
