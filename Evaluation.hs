import qualified Data.Set as S 
import Data.List 
import Data.Function(on)
import Data.Either.Utils
import System.Random 
import Data.Sort 
import Data.List  
import Control.Monad 
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import GSL.Random.Dist 
import GSL.Random.Gen 
import qualified Data.Vector.Storable as V
import qualified Data.Map as M 


type Alpha   = Double 
type Value   = Double 
type Ranking = Integer 
type Alternative = String
type ErrorMsg = String 
type AttrName = String

type Mode = Int -- 0 for equal mode , anything else for random mode

genWeights :: Mode -> Int -> M.Map Int [[Alpha]] -> IO [Alpha]
genWeights m n mlist = do 
      let alpha = (1/fromIntegral n) :: Alpha
      return $ take n (repeat alpha)


genAlphas :: Int -> M.Map Int [[Alpha]] -> IO [Alpha]
genAlphas n mlist = do 
  gen <- newStdGen
  let nlist = (removeJust. M.lookup n) mlist
      rnum = fst $ randomR (0,(length nlist)-1) gen
      removeJust (Just j) = j
  return (nlist !! rnum) 


memoizeWeights :: Ratio -> IO (M.Map Int [[Alpha]])
memoizeWeights r = return $ M.empty  


genAlphasD :: Int -> Double -> IO [Alpha]
genAlphasD n d = do
  v <- newRNG mt19937
  nums <-  getDirichlet v (V.replicate n d)
  --putStrLn $ show nums  
  return $ (reverse. sort. V.toList) nums
              
list = [0.10,0.11..(8.00)] 


evalRatio :: [Double] -> Int -> [[Alpha]] -> Int -> Ratio -> IO (Int,[[Alpha]])
evalRatio [] n ls c r = return (n,ls) 
evalRatio (x:xs) n ls c r = do 
  tr <- testRatio n x 
  let ffst = (floor.fst) tr 
  case c <= 40 of 
    True  -> do 
      case fromIntegral ffst < r && ffst >= 1 of 
        True  -> evalRatio xs n (snd tr:ls) (c+1) r 
        False -> evalRatio xs n ls c r 
    False -> return (n,ls)





-- evalRatio :: [Double] -> Int -> [(Double,Int)] -> IO (Int,[(Double,Int)])
-- evalRatio [] n ls = return (n,ls) 
-- evalRatio (x:xs) n ls = do 
--   tr <- testRatio n x 
--   let ffst = (floor.fst) tr 
--   case ffst < 9 && ffst >= 2 of 
--     True  -> evalRatio xs n ((x,ffst):ls)
--     False -> evalRatio xs n ls 



type Ratio = Double

testRatio :: Int -> Double -> IO (Ratio,[Alpha]) 
testRatio n d = do 
  as <- genAlphasD n d 
  let ratioFun x = (head x)/(last x)
  return $ (ratioFun as,as) 

numGen n = do 
  gen <- newStdGen
  let ns = randomRs (0.01,0.99) gen :: [Double]
  return $ take n ns



genVals :: [Alternative] -> RowSize -> ColSize -> IO [(Alternative,[Value])]
genVals as rs cs = do  
  bs <- numGen (rs*cs)
  let aFun [] 0 _ [] = []
      aFun (a:axs) rys czs bws = (a,take czs bws) : aFun axs (rys-1) czs (drop czs bws)   
  return $ aFun as rs cs bs 



-- genWeightandValues :: [Alternative] -> ColSize -> IO ([Alpha],[(Alternative,[Value])])
-- genWeightandValues as cs = do 
--     alphas <- genAlphas cs 
--     vals   <- genVals as (length as) cs  
--     return (alphas,vals)


genWeightandValues :: [Alternative] -> ColSize -> M.Map Int [[Alpha]] -> IO ([Alpha],[(Alternative,[Value])])
genWeightandValues as cs mlist = do 
    alphas <- genWeights 0 cs mlist
    vals   <- genVals as (length as) cs  
    return (alphas,vals)



type RowSize = Int 
type ColSize = Int 
type Shrinkage = Double 



main = do 
  mlist <- memoizeWeights 9 
  -- xs1 <- testShrCompLength 2000 8 mlist
  xs2 <- testShrCompLength 600 10 mlist
  -- xs3 <- testShrCompLength 2000 14 mlist
  -- xs4 <- testShrCompLength 2000 18 mlist
  toFile def "Srinkage_600.png" $ do
    layout_title .= "Shrinkage (Y-Axis) vs Number of Decomposed Components % (X-Axis) [EQUAL WEIGHTS -- NEW MDS]"
    --mapM_ (\x -> plot (line "" x)) [fs] 
    -- plot (line "No of Alternatives in MADM = 8" [xs1]) 
    plot (line "No of Alternatives in MADM = 10" [xs2])
    -- plot (line "No of Alternatives in MADM = 14" [xs3])
    -- plot (line "No of Alternatives in MADM = 18" [xs4])


testShrinkage :: [Alternative] -> ColSize -> M.Map Int [[Alpha]] -> IO Average
testShrinkage alts csz mlist = do 
    inp <- genWeightandValues alts csz mlist
    let 
      f (x,y) = (explNList. snd. rmvRight. madm_wsm x) y 
    return $ f inp 


testShrinkages :: Int -> [Alternative] -> ColSize -> M.Map Int [[Alpha]] -> IO [Average]
testShrinkages 0 _ _ _ = return []
testShrinkages n as cs mlist = do 
  x  <- testShrinkage as cs mlist
  xs <- testShrinkages (n-1) as cs mlist
  return (x:xs)

testShrMain :: Int -> [Alternative] -> ColSize -> M.Map Int [[Alpha]] -> IO (ColSize,Average)
testShrMain n as cs mlist = do 
    avrs <- testShrinkages n as cs mlist
    return (cs,sum avrs/((fromIntegral.length)avrs))

testShrCompLength :: Int -> RowSize -> M.Map Int [[Alpha]] ->  IO [(ColSize,Average)]
testShrCompLength n rs mlist = do 
    let --cs = [4,5,6,7,8,9,10,18,20,22,28,30,36,42,48,80,90,100,120,140]
        cs = [4,8..600] 
        alts = map (\x -> "A" ++ show x) [1..rs]
    mapM (\x -> testShrMain n alts x mlist) cs   

-- =====================================================================================
-- =====================================================================================
