import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import Evaluation
import GenAHPVals

fitData :: [FocusVal] -> [(AttrCount,FocusFactor)]
fitData = map (\(x,y) -> (snd x,snd y) )

genData :: Ratio -> Levels -> SampleCount -> IO [(AttrCount,FocusFactor)]
genData r l s = do 
    xs <- genTests r l s 
    return $ fitData xs 

genGraph :: Levels -> SampleCount -> IO ()
genGraph l n = do 
    let rs = [1.01,1.05,1.1,1.2,1.3]
        fname = "FigureLevExt" ++ show l ++ ".svg"
        msg r = "" -- "Decision margin ≤ " ++ (show . round) ((r-1)*100) ++ " %"  
        plotFun x r = plot (line (msg r) [x]) :: EC (Layout Int Double) ()
    
    xs <- mapM (\r -> genData r l n) rs 
    toFile def fname $ do
        layout_title .= ""
        setColors [opaque darkred, opaque green,opaque blue,opaque black, opaque orange]
        plotFun (xs!!0) (rs!!0) 
        plotFun (xs!!1) (rs!!1) 
        plotFun (xs!!2) (rs!!2) 
        plotFun (xs!!3) (rs!!3) 
        plotFun (xs!!4) (rs!!4) 
    
    return ()
     

-- "MDS reduction (y-axis, in %), depending on number of components (x-axis): " ++ show l ++ " levels"

main = do 
   --xs <- genData 1.01 3 1  
   --putStrLn $ show xs 
      -- genGraph 3 20000
      genGraph 3 20000
      --genGraph 3 1000
      --enGraph 6 20000
  --genGraph 5 200
  -- genGraph 5 20
  -- genGraph 6 20




-- =====================================================================================
-- =====================================================================================
