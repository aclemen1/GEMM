module EMM.Types
  ( Group(..)
  , GradedGroup(..)
  , emptyGroup
  , singletonGroup
  , addToGroup
  , groupInDegree
  , anticonnexity
  , showGroup
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

-- | A Group is a direct sum of powered cyclic groups.
--   Represented as IntMap from logorder to power.
--   logorder=0 means Z (infinite cyclic), logorder=k>0 means Z/2^k.
newtype Group = Group { unGroup :: IntMap Int }
  deriving (Eq, Show)

-- | A GradedGroup maps degree to Group.
newtype GradedGroup = GradedGroup { unGraded :: IntMap Group }
  deriving (Eq, Show)

instance Semigroup Group where
  Group m1 <> Group m2 = Group (IM.unionWith (+) m1 m2)

instance Monoid Group where
  mempty = Group IM.empty

instance Semigroup GradedGroup where
  GradedGroup m1 <> GradedGroup m2 = GradedGroup (IM.unionWith (<>) m1 m2)

instance Monoid GradedGroup where
  mempty = GradedGroup IM.empty

emptyGroup :: Group
emptyGroup = mempty

singletonGroup :: Int -> Int -> Group
singletonGroup logorder power
  | power == 0 = emptyGroup
  | otherwise  = Group (IM.singleton logorder power)

addToGroup :: Int -> Int -> Group -> Group
addToGroup logorder power g
  | power == 0 = g
  | otherwise  = g <> singletonGroup logorder power

groupInDegree :: Int -> GradedGroup -> Group
groupInDegree deg (GradedGroup m) = IM.findWithDefault emptyGroup deg m

anticonnexity :: GradedGroup -> Int
anticonnexity (GradedGroup m)
  | IM.null m = 0
  | otherwise = fst (IM.findMax m)

showGroup :: Group -> String
showGroup (Group m)
  | IM.null m = "(0)"
  | otherwise = concatWith " + " $ map showComponent (IM.toAscList m)
  where
    showComponent (0, p)
      | p == 1    = "Z"
      | otherwise = "Z^" ++ show p
    showComponent (l, p)
      | p == 1    = "Z/2^" ++ show l
      | otherwise = "(Z/2^" ++ show l ++ ")^" ++ show p
    concatWith _ []     = ""
    concatWith _ [x]    = x
    concatWith sep (x:xs) = x ++ sep ++ concatWith sep xs
