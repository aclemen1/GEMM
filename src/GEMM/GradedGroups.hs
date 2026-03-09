-- |
-- Module      : GEMM.GradedGroups
-- Description : Algebraic operations on groups and graded groups.
--
-- Implements the four fundamental bifunctors on finitely generated abelian
-- groups (tensor, Tor, Hom, Ext), their bilinear extensions to 'Group',
-- and the two key tools for computing (co)homology:
--
--   * The Künneth formula for graded groups.
--   * The Universal Coefficients Theorem.
--
-- == Formulas for cyclic groups
--
-- For cyclic groups Z/p^a and Z/p^b (same prime p):
--
--   * @Z/p^a ⊗ Z/p^b = Z/p^min(a,b)@
--   * @Tor(Z/p^a, Z/p^b) = Z/p^min(a,b)@
--   * @Hom(Z/p^a, Z/p^b) = Z/p^min(a,b)@
--   * @Ext(Z/p^a, Z/p^b) = Z/p^min(a,b)@
--
-- For the free group Z:
--
--   * @Z ⊗ Z/p^b = Z/p^b@,  @Tor(Z, _) = 0@
--   * @Hom(Z, Z/p^b) = Z/p^b@,  @Ext(Z, _) = 0@
--   * @Hom(Z/p^a, Z) = 0@,  @Ext(Z/p^a, Z) = Z/p^a@
--
-- For groups at different primes p ≠ q:
--
--   * @Z/p^a ⊗ Z/q^b = 0@ (coprime orders)
--   * @Tor(Z/p^a, Z/q^b) = 0@
--   * @Hom(Z/p^a, Z/q^b) = 0@
--   * @Ext(Z/p^a, Z/q^b) = 0@
module GEMM.GradedGroups
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

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import GEMM.Types

-- | Tensor product of two powered cyclic group components.
--
-- Each component is represented as @((prime, logorder), power)@.
-- The prime=0 convention means Z (infinite cyclic).
-- Components at different primes tensor to zero.
tensor :: ((Int,Int), Int) -> ((Int,Int), Int) -> ((Int,Int), Int)
tensor ((0,0), p1)  ((0,0), p2)  = ((0,0), p1 * p2)         -- Z ⊗ Z = Z
tensor ((0,0), p1)  ((q,l), p2)  = ((q,l), p1 * p2)         -- Z ⊗ Z/q^l = Z/q^l
tensor ((q,l), p1)  ((0,0), p2)  = ((q,l), p1 * p2)         -- Z/q^l ⊗ Z = Z/q^l
tensor ((p,a), p1)  ((q,b), p2)
  | p /= q    = ((0,0), 0)                                    -- different primes: 0
  | otherwise  = ((p, min a b), p1 * p2)                       -- Z/p^a ⊗ Z/p^b = Z/p^min(a,b)

-- | Tor functor of two powered cyclic group components.
--
-- @Tor(Z, _) = 0@, @Tor(_, Z) = 0@.
-- @Tor(Z/p^a, Z/p^b) = Z/p^min(a,b)@ for same prime.
-- Different primes give 0.
tor :: ((Int,Int), Int) -> ((Int,Int), Int) -> ((Int,Int), Int)
tor ((0,0), _)   _              = ((0,0), 0)                  -- Tor(Z, _) = 0
tor _            ((0,0), _)     = ((0,0), 0)                  -- Tor(_, Z) = 0
tor ((p,a), p1)  ((q,b), p2)
  | p /= q    = ((0,0), 0)                                    -- different primes: 0
  | otherwise  = ((p, min a b), p1 * p2)

-- | Hom functor of two powered cyclic group components.
--
-- @Hom(Z, Z/p^b) = Z/p^b@, @Hom(Z, Z) = Z@.
-- @Hom(Z/p^a, Z) = 0@.
-- @Hom(Z/p^a, Z/p^b) = Z/p^min(a,b)@ for same prime.
hom :: ((Int,Int), Int) -> ((Int,Int), Int) -> ((Int,Int), Int)
hom ((0,0), p1) ((k2), p2)     = (k2, p1 * p2)               -- Hom(Z, M) = M
hom _           ((0,0), _)     = ((0,0), 0)                   -- Hom(Z/p^a, Z) = 0
hom ((p,a), p1) ((q,b), p2)
  | p /= q    = ((0,0), 0)                                    -- different primes: 0
  | otherwise  = ((p, min a b), p1 * p2)

-- | Ext functor of two powered cyclic group components.
--
-- @Ext(Z, _) = 0@.
-- @Ext(Z/p^a, Z) = Z/p^a@.
-- @Ext(Z/p^a, Z/p^b) = Z/p^min(a,b)@ for same prime.
ext :: ((Int,Int), Int) -> ((Int,Int), Int) -> ((Int,Int), Int)
ext ((0,0), _)  _              = ((0,0), 0)                   -- Ext(Z, _) = 0
ext ((p,a), p1) ((0,0), _)    = ((p,a), p1)                   -- Ext(Z/p^a, Z) = Z/p^a
ext ((p,a), p1) ((q,b), p2)
  | p /= q    = ((0,0), 0)                                    -- different primes: 0
  | otherwise  = ((p, min a b), p1 * p2)

-- | Bilinear extension of a bifunctor on cyclic components to 'Group'.
--
-- Given a function @f@ on pairs of cyclic components, iterate over all
-- pairs from two groups and sum the results.
bilinearG :: (((Int,Int), Int) -> ((Int,Int), Int) -> ((Int,Int), Int))
          -> Group -> Group -> Group
bilinearG f (Group m1) (Group m2) =
  Map.foldlWithKey' (\acc k1 p1 ->
    Map.foldlWithKey' (\acc' k2 p2 ->
      let (k, p) = f (k1, p1) (k2, p2)
      in if p == 0 then acc'
         else acc' <> Group (Map.singleton k p)
    ) acc m2
  ) emptyGroup m1

-- | Tensor product extended to groups.
tensorG :: Group -> Group -> Group
tensorG = bilinearG tensor

-- | Tor functor extended to groups.
torG :: Group -> Group -> Group
torG = bilinearG tor

-- | Hom functor extended to groups.
homG :: Group -> Group -> Group
homG = bilinearG hom

-- | Ext functor extended to groups.
extG :: Group -> Group -> Group
extG = bilinearG ext

-- | Künneth formula for graded groups (doubly sparse, bounded variant).
--
-- For graded groups @H@ and @H'@, the Künneth formula gives:
--
-- @H_n(X × Y) = ⊕_{i+j=n} (H_i(X) ⊗ H_j(Y)) ⊕ ⊕_{i+j=n-1} Tor(H_i(X), H_j(Y))@
--
-- The @maxDeg@ parameter bounds the computation: only degrees @≤ maxDeg@
-- are produced.  Since both input lists are sorted by ascending degree,
-- 'takeWhile' provides early termination on the inner loop.
kunneth :: Int -> GradedGroup -> GradedGroup -> GradedGroup
kunneth maxDeg (GradedGroup m1) (GradedGroup m2) =
  let entries1 = IM.toAscList m1
      entries2 = IM.toAscList m2
  in mconcat
       [ singletonGG (i + j) (tensorG g1 g2)
         <> if i + j < maxDeg
            then singletonGG (i + j + 1) (torG g1 g2)
            else mempty
       | (i, g1) <- entries1
       , i <= maxDeg
       , (j, g2) <- takeWhile (\(j', _) -> i + j' <= maxDeg) entries2
       ]

-- | Universal Coefficients Theorem for integral cohomology.
--
-- @H^n(X; Z) ≅ Hom(H_n(X), Z) ⊕ Ext(H_{n-1}(X), Z)@
universalCoefficients :: GradedGroup -> GradedGroup
universalCoefficients gg =
  let ac = anticonnexity gg
      z  = freeGroup 1
  in foldl (\acc n ->
       let extPart = singletonGG n (extG (groupInDegree (n - 1) gg) z)
           homPart = singletonGG n (homG (groupInDegree n gg) z)
       in acc <> extPart <> homPart
     ) mempty [0..ac]

-- | Helper: create a 'GradedGroup' with a single 'Group' at a given degree.
-- Returns 'mempty' for the trivial group.
singletonGG :: Int -> Group -> GradedGroup
singletonGG _ (Group m) | Map.null m = mempty
singletonGG deg g = GradedGroup (IM.singleton deg g)
