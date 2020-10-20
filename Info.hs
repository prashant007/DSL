module Info where

-- import Prelude hiding (compare)
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe (fromJust)
import Text.Printf
    
import Record


-- Types that can automatically enumerate their members
--
class (Bounded a,Enum a,Ord a) => Set a where
  members :: [a]
  members = enumFromTo minBound maxBound


-- Tagged numbers
--
type Nums a = [(a,Double)]

data Info o a = Info {unInfo :: M.Map o (Rec a)}

mkInfo :: Ord o => [(o,Rec a)] -> Info o a
mkInfo = Info . M.fromList

info :: (Ord o,Ord a) => [(o,Nums a)] -> Info o a
info ons = mkInfo [(o,mkRec ns) | (o,ns) <- ons]

fromInfo :: Info o a -> [(o,Rec a)]
fromInfo = M.toList . unInfo

-- loookup in a record
lookupInfo :: (Ord o,Ord a) => (o,a) -> Info o a -> Double
lookupInfo (o,a)  = lookupRec a . fromJust . M.lookup o . unInfo 

onInfo :: (M.Map o (Rec a) -> M.Map o (Rec b)) -> Info o a -> Info o b
onInfo f = Info . f . unInfo

insertInfo :: Ord o => o -> Rec a -> Info o a -> Info o a 
insertInfo o r = onInfo (M.insert o r)

onInfo2 :: (M.Map o (Rec a) -> M.Map o (Rec b) -> M.Map o (Rec c)) ->
           Info o a -> Info o b -> Info o c
onInfo2 f i j = Info $ f (unInfo i) (unInfo j)

mapInfo :: Ord a => (Rec a -> Rec b) -> Info o a -> Info o b
mapInfo =  onInfo . M.map

objects :: (Set o,Ord a) => Info o a
objects = mkInfo [(o,emptyRec) | o <- members]

select :: Ord o => o -> Info o a -> Rec a
select o = fromJust . M.lookup o . unInfo

(!) :: Ord o => Info o a -> o -> Rec a
(!) = flip select

diff :: (Ord o,Ord r) => Info o r -> o -> o -> Rec r
diff i o1 o2 = i!o1 - i!o2

showPairLn :: (Show a,Show b) => (a,b) -> String
showPairLn (x,y) = show x ++ " -> \n" ++ show y

instance (Show o,Show a) => Show (Info o a) where
  show = showSetLn . map showPairLn . fromInfo

addAttribute :: (Ord o,Ord a) => a -> Nums o -> Info o a -> Info o a
addAttribute c as bs = mkInfo [(b,insertRec c av bv) | (a,av) <- as,(b,bv) <- fromInfo bs,a == b]

gather :: (Set o,Set a) => (a -> Nums o) -> Info o a
gather f = foldl (\o a -> addAttribute a (f a) o) objects members

addAlternative :: (Ord o,Set a) => o -> (a -> Double) -> Info o a -> Info o a
addAlternative o f = insertInfo o (mkRec ls) 
    where ls = map (\x -> (x,f x)) members

union :: (Ord o,Set a) => Info o a -> Info o a -> Info o a
union = onInfo2 M.union

-- a function for removing an attribute
delAttribute :: (Ord o,Ord a) => a -> Info o a -> Info o a
delAttribute a = onInfo (M.map (deleteRec a)) 

-- a function for modifying an attribute 
modAttribute :: (Ord a,Ord o) => a -> o -> Double -> Info o a -> Info o a
modAttribute a o' v = onInfo (M.mapWithKey modRec) 
    where modRec o ov 
              | o == o'= insertRec a v (deleteRec a ov) 
              | otherwise = ov 

-- a function for removing a dimension
delDim :: (Ord a,Ord o) => Info o a -> [a] -> Info o a
delDim = foldl (flip delAttribute)

transpose :: (Ord a,Ord o) => Info o a -> Info a o
transpose i = mkInfo $ map (\x -> (x, (mkRec . toNums x) i)) (allAttrs i)

toNums :: (Ord a,Ord o) => a -> Info o a -> Nums o
toNums a = map (\(o,l) -> (o,lookupRec a l)) . fromInfo

allAttrs :: Info o a -> [a]
allAttrs = M.keys . unRec . snd . head . fromInfo

listToInfo :: (Ord o,Ord a) => [((o,a),Double)] -> Info o a
listToInfo = Info . M.map assocRec . groupByKeys fst . M.fromList
  where assocRec = Rec . M.mapKeys snd 
