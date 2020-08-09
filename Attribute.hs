module Attribute where

import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.Function (on)
import Data.List
import Text.Printf


class (Bounded a,Enum a,Ord a) => Set a where
  members :: [a]
  members = enumFromTo minBound maxBound

-- Used to represent beneficial or non-beneficial attributes
--
data Valence = Pos | Neg

class Ord a => AttrValence a where
   valence :: a -> Valence
   valence _ = Pos

data Attr a = Attr {unAttr :: M.Map a Double}

mkAttr :: Ord a => [(a,Double)] -> Attr a
mkAttr = Attr . M.fromList

fromAttr :: Attr a -> [(a,Double)]
fromAttr = M.toList . unAttr

onAttr :: Ord a => (Double -> Double -> Double) -> Attr a -> Attr a -> Attr a
onAttr g x y =  Attr $ merge preserveMissing preserveMissing
                       (zipWithMatched (\_->g)) (unAttr x) (unAttr y)


-- Needed?
--
{-
add :: Ord a => Attr a -> Attr a -> Attr a
add = onAttr (+)
-}

diff :: Ord a => Attr a -> Attr a -> Attr a
diff = onAttr (-)

instance Show a => Show (Attr a) where
  show ts = let ts' = map (\(x,y) -> show x ++ " -> " ++ printf "%.3f" y) (fromAttr ts)
            in "{" ++ intercalate ",\n " ts' ++ "}"

(-->) :: a -> v -> (a,v)
x --> y = (x,y)

-- Attribute value distribution
--
type Spread o = [(o,Double)]

noAttributes :: Ord a => Attr a
noAttributes = mkAttr []

