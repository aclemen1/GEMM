-- |
-- Module      : GEMM.ElementaryComplex
-- Description : Homology of elementary complexes in the sense of Cartan.
--
-- An elementary complex of type (II) is a tensor product of two type (I)
-- complexes: either @E(x, 2q-1) ⊗ P(y, 2q)@ or @P(x, 2q) ⊗ E(y, 2q+1)@,
-- equipped with a differential @d@ of order @h@ (an integer).
--
-- == Homology (Lemme 2.3, Tischler)
--
-- For @E(x, 2q-1) ⊗ P(y, 2q)@ with differential of order h:
--
--   * Cycles: @x ⊗ γ_ℓ(y)@ in degree @(ℓ+1) deg(x) + ℓ@, class of order h.
--   * Equivalently: in degree m with @deg | m@, the group is Z/h·ν(m/deg)
--     where ν is the appropriate p-adic valuation.
--
-- For @P(x, 2q) ⊗ E(y, 2q+1)@ with differential of order h:
--
--   * Cycles: @γ_ℓ(x) ⊗ 1@ in degree @ℓ deg(x)@, class of order ℓh.
--   * In degree m with @(deg+1) | (m+1)@, the group is Z/h (constant order).
--
-- == Parameters
--
-- The function 'ecHomology' takes:
--
--   * @p@ — the prime (used for p-adic valuation in even-degree case).
--   * @degree@ — the degree of the generator of the elementary complex.
--   * @logh@ — the log (base p) of the differential's order h = p^logh.
--   * @ac@ — upper bound (anticonnexity) for the computation.
module GEMM.ElementaryComplex
  ( nuP
  , nu2
  , ecHomology
  , ecCohomology
  ) where

import qualified Data.IntMap.Strict as IM
import GEMM.Types
import GEMM.GradedGroups (universalCoefficients)

-- | p-adic valuation: the largest k such that p^k divides n.
--
-- >>> nuP 2 12
-- 2
-- >>> nuP 3 27
-- 3
-- >>> nuP 5 100
-- 2
nuP :: Int -> Int -> Int
nuP _ 0 = error "nuP: zero"
nuP p n = go (abs n) 0
  where
    go x k
      | x `mod` p == 0 = go (x `div` p) (k + 1)
      | otherwise       = k

-- | 2-adic valuation. Equivalent to @nuP 2@, kept for backward compatibility.
nu2 :: Int -> Int
nu2 = nuP 2

-- | Compute the integral homology of an elementary complex.
--
-- @ecHomology p degree logh ac@ computes H_*(elementary complex) where:
--
--   * @p@ is the prime for p-adic valuation (relevant for even-degree case).
--   * @degree@ is the degree of the generator pair.
--   * @logh@ is the log-order of the differential: h = p^logh.
--   * @ac@ is the upper bound for degrees to compute.
--
-- For even @degree@: the complex is P(x, degree) ⊗ E(y, degree+1).
-- Homology in degree m (where degree | m) is Z/p^(logh + ν_p(m/degree)).
--
-- For odd @degree@: the complex is E(x, degree) ⊗ P(y, degree+1).
-- Homology in degree m (where (degree+1) | (m+1)) is Z/p^logh.
ecHomology :: Int -> Int -> Int -> Int -> GradedGroup
ecHomology p degree logh ac =
  let -- Degree 0 always has Z (the base point).
      z0 = GradedGroup (IM.singleton 0 (freeGroup 1))
      cells = IM.fromListWith (<>)
        [ (m, singletonGroup p logord 1)
        | m <- [1..ac]
        , validDegree degree m
        , let logord = computeLogorder p degree logh m
        ]
  in z0 <> GradedGroup cells
  where
    -- | For even degree d: homology appears in degrees that are multiples of d.
    --   For odd degree d: homology appears in degrees m where (d+1) | (m+1).
    validDegree deg m
      | even deg  = m `mod` deg == 0
      | otherwise = (m + 1) `mod` (deg + 1) == 0

    -- | Compute the logorder (exponent of p) of the cyclic group in degree m.
    --   Even degree: logh + ν_p(m/deg), giving increasing p-torsion.
    --   Odd degree: constant logh.
    computeLogorder prime deg lh m
      | even deg  = lh + nuP prime (m `div` deg)
      | otherwise = lh

-- | Integral cohomology of an elementary complex, via the Universal
-- Coefficients Theorem applied to 'ecHomology'.
ecCohomology :: Int -> Int -> Int -> Int -> GradedGroup
ecCohomology p degree logh ac = universalCoefficients (ecHomology p degree logh ac)
