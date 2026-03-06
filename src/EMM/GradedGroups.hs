module EMM.GradedGroups
  ( tensor
  , tor
  , hom
  , ext
  , tensorG
  , torG
  , homG
  , extG
  , kunneth
  , universalCoefficients
  ) where

import qualified Data.IntMap.Strict as IM
import EMM.Types

-- Tensor product of powered cyclic groups
tensor :: (Int, Int) -> (Int, Int) -> (Int, Int)
tensor (l1, p1) (l2, p2) = (lo, p1 * p2)
  where
    lo | l1 == 0   = l2
       | l2 == 0   = l1
       | otherwise  = min l1 l2

-- Tor of powered cyclic groups
tor :: (Int, Int) -> (Int, Int) -> (Int, Int)
tor (l1, p1) (l2, p2)
  | l1 == 0 || l2 == 0 = (min l1 l2, 0)
  | otherwise           = (min l1 l2, p1 * p2)

-- Hom of powered cyclic groups
hom :: (Int, Int) -> (Int, Int) -> (Int, Int)
hom (0,  _)  (l2, p2) = (l2, p2)
hom (_,  _)  (0,  _)  = (0, 0)
hom (l1, p1) (l2, p2) = (min l1 l2, p1 * p2)

-- Ext of powered cyclic groups
ext :: (Int, Int) -> (Int, Int) -> (Int, Int)
ext (0,  _)  _        = (0, 0)
ext (l1, p1) (0,  _)  = (l1, p1)
ext (l1, p1) (l2, p2) = (min l1 l2, p1 * p2)

-- Bilinear extension to Group
bilinearG :: ((Int, Int) -> (Int, Int) -> (Int, Int)) -> Group -> Group -> Group
bilinearG f (Group m1) (Group m2) =
  IM.foldlWithKey' (\acc l1 p1 ->
    IM.foldlWithKey' (\acc' l2 p2 ->
      let (l, p) = f (l1, p1) (l2, p2)
      in addToGroup l p acc'
    ) acc m2
  ) emptyGroup m1

tensorG :: Group -> Group -> Group
tensorG = bilinearG tensor

torG :: Group -> Group -> Group
torG = bilinearG tor

homG :: Group -> Group -> Group
homG = bilinearG hom

extG :: Group -> Group -> Group
extG = bilinearG ext

-- Kunneth formula for graded groups
kunneth :: GradedGroup -> GradedGroup -> GradedGroup
kunneth gg1 gg2 =
  let ac = max (anticonnexity gg1) (anticonnexity gg2)
  in foldl (\acc n ->
       let tensorPart = mconcat
             [ singletonGG n (tensorG (groupInDegree i gg1) (groupInDegree (n - i) gg2))
             | i <- [0..n] ]
           torPart = mconcat
             [ singletonGG n (torG (groupInDegree i gg1) (groupInDegree (n - 1 - i) gg2))
             | i <- [0..n-1] ]
       in acc <> tensorPart <> torPart
     ) mempty [0..ac]

-- Universal coefficients theorem
universalCoefficients :: GradedGroup -> GradedGroup
universalCoefficients gg =
  let ac = anticonnexity gg
      z  = singletonGroup 0 1
  in foldl (\acc n ->
       let extPart = singletonGG n (extG (groupInDegree (n - 1) gg) z)
           homPart = singletonGG n (homG (groupInDegree n gg) z)
       in acc <> extPart <> homPart
     ) mempty [0..ac]

-- Helper: create a GradedGroup with a single group at a degree
singletonGG :: Int -> Group -> GradedGroup
singletonGG _ (Group m) | IM.null m = mempty
singletonGG deg g = GradedGroup (IM.singleton deg g)
