-- |
-- Module      : GEMM.Types
-- Description : Core algebraic types for the Eilenberg-MacLane Machine.
--
-- This module defines the fundamental types used throughout the EMM:
--
--   * 'Group' — a finitely generated abelian group, represented as a direct sum
--     of powered cyclic groups Z/p^k (torsion) and Z (free part).
--
--   * 'GradedGroup' — a graded abelian group, mapping integer degrees to 'Group's.
--
-- == Representation
--
-- A 'Group' is stored as @Map (Int, Int) Int@ where:
--
--   * Key @(0, 0)@ with value @r@ represents the free part Z^r.
--   * Key @(p, k)@ with @p > 0@, @k > 0@ and value @m@ represents (Z\/p^k)^m.
--
-- This allows groups with torsion at multiple primes, as needed for
-- K(Z, n) where all primes contribute.
--
-- The previous representation used @IntMap Int@ with implicit prime p=2.
-- The new representation is a strict superset: it handles any prime and
-- mixed-prime torsion.
module GEMM.Types
  ( Group(..)
  , GradedGroup(..)
  , emptyGroup
  , singletonGroup
  , freeGroup
  , addToGroup
  , addFreeRank
  , groupInDegree
  , anticonnexity
  , showGroup
  ) where

import Control.DeepSeq (NFData(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)

-- | A finitely generated abelian group, represented as a direct sum of
-- powered cyclic groups.
--
-- Internally stored as @Map (prime, logorder) power@:
--
--   * @(0, 0) -> r@ means Z^r (free part of rank r).
--   * @(p, k) -> m@ with p prime, k >= 1 means (Z/p^k)^m.
--
-- The 'Semigroup' instance implements direct sum: powers are added
-- component-wise. The 'Monoid' instance gives the trivial group.
newtype Group = Group { unGroup :: Map (Int, Int) Int }
  deriving (Eq, Show)

instance NFData Group where
  rnf (Group m) = rnf m

-- | A graded abelian group mapping integer degrees to 'Group's.
--
-- The 'Semigroup' instance merges groups degree-wise using direct sum.
newtype GradedGroup = GradedGroup { unGraded :: IntMap Group }
  deriving (Eq, Show)

instance NFData GradedGroup where
  rnf (GradedGroup m) = rnf m

-- | Direct sum of groups: merge by adding powers of each component.
instance Semigroup Group where
  Group m1 <> Group m2 = Group (Map.unionWith (+) m1 m2)

instance Monoid Group where
  mempty = Group Map.empty

-- | Merge graded groups degree-wise.
instance Semigroup GradedGroup where
  GradedGroup m1 <> GradedGroup m2 = GradedGroup (IM.unionWith (<>) m1 m2)

instance Monoid GradedGroup where
  mempty = GradedGroup IM.empty

-- | The trivial group (0).
emptyGroup :: Group
emptyGroup = mempty

-- | Create a group with a single torsion component (Z/p^k)^power.
--
-- @singletonGroup p k power@ represents (Z\/p^k)^power.
-- Returns 'emptyGroup' if power is 0.
--
-- For backward compatibility with the old p=2 interface:
-- @singletonGroup 2 k m@ is equivalent to the old @singletonGroup k m@.
singletonGroup :: Int -> Int -> Int -> Group
singletonGroup _ _ 0     = emptyGroup
singletonGroup p k power = Group (Map.singleton (p, k) power)

-- | Create a free abelian group Z^r.
--
-- @freeGroup r@ represents Z^r. Returns 'emptyGroup' if r is 0.
freeGroup :: Int -> Group
freeGroup 0 = emptyGroup
freeGroup r = Group (Map.singleton (0, 0) r)

-- | Add a torsion component to an existing group.
--
-- @addToGroup p k power g@ adds (Z\/p^k)^power to g.
addToGroup :: Int -> Int -> Int -> Group -> Group
addToGroup _ _ 0 g     = g
addToGroup p k power g = g <> singletonGroup p k power

-- | Add free rank to an existing group.
--
-- @addFreeRank r g@ adds Z^r to g.
addFreeRank :: Int -> Group -> Group
addFreeRank 0 g = g
addFreeRank r g = g <> freeGroup r

-- | Look up the group at a given degree in a graded group.
-- Returns 'emptyGroup' if the degree is absent.
groupInDegree :: Int -> GradedGroup -> Group
groupInDegree deg (GradedGroup m) = IM.findWithDefault emptyGroup deg m

-- | The highest degree with a non-trivial group.
-- Returns 0 for the trivial graded group.
anticonnexity :: GradedGroup -> Int
anticonnexity (GradedGroup m)
  | IM.null m = 0
  | otherwise = fst (IM.findMax m)

-- | Human-readable display of a group.
--
-- Examples:
--
-- >>> showGroup emptyGroup
-- "(0)"
-- >>> showGroup (freeGroup 1)
-- "Z"
-- >>> showGroup (singletonGroup 2 3 2)
-- "(Z/2^3)^2"
-- >>> showGroup (freeGroup 1 <> singletonGroup 3 1 2)
-- "Z + (Z/3)^2"
showGroup :: Group -> String
showGroup (Group m)
  | Map.null m = "(0)"
  | otherwise  = intercalate " + " $ map showComponent (Map.toAscList m)
  where
    -- Free part: Z or Z^r
    showComponent ((0, 0), r)
      | r == 1    = "Z"
      | otherwise = "Z^" ++ show r
    -- Torsion part: Z/p^k or (Z/p^k)^m
    showComponent ((p, k), power) =
      let base | k == 1    = "Z/" ++ show p
               | otherwise = "Z/" ++ show p ++ "^" ++ show k
          wrapped | power > 1 = "(" ++ base ++ ")^" ++ show power
                  | otherwise = base
      in wrapped
