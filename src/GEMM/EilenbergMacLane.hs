-- |
-- Module      : GEMM.EilenbergMacLane
-- Description : Computation of integral (co)homology of Eilenberg-MacLane spaces.
--
-- This module implements the main algorithm of the Eilenberg-MacLane Machine.
-- It computes the integral homology H_*(K(G, n); Z) for three families of groups:
--
--   * @K(Z/2^s, n)@ — the original case (Clément's thesis, Appendix B).
--   * @K(Z/p^f, n)@ — generalization to any prime p (Tischler, Chapter II).
--   * @K(Z, n)@ — Eilenberg-MacLane spaces for the integers (all primes contribute).
--
-- == Algorithm overview
--
-- The algorithm is based on Cartan's théorème fondamental (Théorème 2.7, Tischler):
--
-- 1. Start with the base elementary complex X'(p) (for K(Z/p^f, n)) or X(0) (for K(Z, n)).
-- 2. For each admissible sequence of increasing stable degree:
--    a. Generate the corresponding elementary complex.
--    b. Apply the Künneth formula to accumulate the homology.
-- 3. For K(Z, n): repeat step 2 for each prime p ≤ (range - n) / 2 + 1,
--    then combine the free part from X(0) with all p-primary parts.
--
-- == References
--
-- Cartan, H. — Séminaire H. Cartan, 1954-55, Exposés 1-13.
-- Clément, A. — Integral Cohomology of Finite Postnikov Towers, Appendix B.
-- Tischler, N. — Chapter II, Propositions 2.11-2.13, Corollaire 2.15.
module GEMM.EilenbergMacLane
  ( -- * Original interface (p = 2)
    emHomology
  , emHomologyWithGenerators
  , emCohomology
    -- * Generalized interface (any prime p)
  , emHomologyP
  , emHomologyPWithGenerators
  , emCohomologyP
    -- * K(Z, n) interface
  , emHomologyZ
  , emHomologyZWithGenerators
  , emCohomologyZ
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import GEMM.Types
import GEMM.GradedGroups (kunneth, universalCoefficients)
import GEMM.AdmissibleSequences
  ( AdmissibleSeq(..), stableDegree, genus
  , generateSequencesP, filterByEvenFirst )
import GEMM.ElementaryComplex (ecHomology)

-- ---------------------------------------------------------------------------
-- Original interface for K(Z/2^s, n) — backward compatible
-- ---------------------------------------------------------------------------

-- | Compute the integral homology of K(Z/2^s, n) up to given range.
--
-- This is the original EMM function, kept for backward compatibility.
-- Equivalent to @emHomologyP 2 s n range@.
emHomology :: Int -> Int -> Int -> GradedGroup
emHomology s n range_ = emHomologyP 2 s n range_

-- | Compute homology of K(Z/2^s, n) and collect the generator sequences.
emHomologyWithGenerators :: Int -> Int -> Int -> (GradedGroup, [AdmissibleSeq])
emHomologyWithGenerators s n range_ = emHomologyPWithGenerators 2 s n range_

-- | Integral cohomology of K(Z/2^s, n) via Universal Coefficients.
emCohomology :: Int -> Int -> Int -> GradedGroup
emCohomology s n range_ = universalCoefficients (emHomology s n range_)

-- ---------------------------------------------------------------------------
-- Generalized interface for K(Z/p^f, n) — any prime p
-- ---------------------------------------------------------------------------

-- | Compute the integral homology of K(Z/p^f, n) up to given range.
--
-- @emHomologyP p f n range@ computes H_*(K(Z/p^f, n); Z) for degrees 0..range.
--
-- The algorithm (Tischler, §II.2; Clément, Appendix B):
--
-- 1. Start with the base elementary complex X'(p), which has:
--    * Generator pair @(σ^n u, σ^{n-1} ψ_{p,f} u)@ of degree n.
--    * Homology: Z/p^f in degrees that are multiples of n (n even)
--      or congruent to n mod (n+1) (n odd).
--
-- 2. For each stable degree sd = 0, 1, ..., (range - n) / (p - 1):
--    * Generate all p-admissible sequences (a_i ≥ p·a_{i+1}) with
--      that stable degree and excess in [0, n-1], filtered to even first entry.
--    * For each valid sequence, take the Künneth product with the
--      elementary complex of degree @(p-1)·sd + n@ and log-height 1.
--      (For p = 2, this reduces to @sd + n@.)
--
-- 3. Return the accumulated graded group.
emHomologyP :: Int -> Int -> Int -> Int -> GradedGroup
emHomologyP p f n range_ = fst (emHomologyPWithGenerators p f n range_)

-- | Compute homology of K(Z/p^f, n) and collect the generator sequences used.
--
-- The second component of the result is the list of admissible sequences
-- that contributed to the computation (useful for LaTeX rendering of generators).
emHomologyPWithGenerators :: Int -> Int -> Int -> Int -> (GradedGroup, [AdmissibleSeq])
emHomologyPWithGenerators p f n range_ =
  let -- Base elementary complex X'(p): differential of order p^f.
      base = ecHomology p n f range_
      sdZero = AdmissibleSeq [0]
      -- Iterate over stable degrees, accumulating Künneth products.
      -- Use generateSequencesP p for admissibility condition a_i >= p * a_{i+1}.
      -- The degree of the elementary complex for prime p is (p-1)*sd + n,
      -- which generalizes the p=2 formula sd + n (cf. Tischler, Lemme 2.9).
      (result, gens) = foldl (\(gg, gs) sd ->
          let seqs = filterByEvenFirst (generateSequencesP p sd 0 (n - 1))
              ecDeg sq = (p - 1) * stableDegree sq + n
          in foldl (\(gg', gs') sq ->
               if stableDegree sq >= 0
               then ( kunneth gg' (ecHomology p (ecDeg sq) 1 range_)
                    , gs' ++ [sq] )
               else (gg', gs')
             ) (gg, gs) seqs
        ) (base, [sdZero]) [0 .. (range_ - n) `div` max 1 (p - 1)]
  in (result, gens)

-- | Integral cohomology of K(Z/p^f, n) via Universal Coefficients.
emCohomologyP :: Int -> Int -> Int -> Int -> GradedGroup
emCohomologyP p f n range_ = universalCoefficients (emHomologyP p f n range_)

-- ---------------------------------------------------------------------------
-- K(Z, n) — Eilenberg-MacLane spaces for the integers
-- ---------------------------------------------------------------------------

-- | Compute the integral homology of K(Z, n) up to given range.
--
-- @emHomologyZ n range@ computes H_*(K(Z, n); Z) for degrees 0..range.
--
-- From Proposition 2.11 (Tischler):
--
-- 1. The free part comes from X(0):
--    * n even: X(0) = P(σ^n u, n), so H_m = Z when n | m, and 0 otherwise.
--      (This is the homology of CP^∞ for n=2, HP^∞ for n=4, etc.)
--    * n odd: X(0) = E(σ^n u, n), so H_m = Z for m ∈ {0, n}, and 0 otherwise.
--
-- 2. The p-primary torsion for each prime p comes from X''(p):
--    * Only primes p ≤ (range - n) / 2 + 1 can contribute (Prop. 2.11(ii)).
--    * For each such prime, the computation is analogous to K(Z/p^f, n)
--      but using only X''(p) (no X'(p) or X'''(p) since Z has no torsion
--      and no ψ_{p,f} generators).
--    * The elementary complexes in X''(p) have generators of the form
--      (σ^{k+1} γ_p α u, σ^k φ_p α u) where α is a first-kind p-word.
--
-- 3. The results for all primes are combined by direct sum at each degree.
emHomologyZ :: Int -> Int -> GradedGroup
emHomologyZ n range_ = fst (emHomologyZWithGenerators n range_)

-- | Compute the integral homology of K(Z, n) and collect generators per prime.
emHomologyZWithGenerators :: Int -> Int -> (GradedGroup, [(Int, [AdmissibleSeq])])
emHomologyZWithGenerators n range_ =
  let freePart = freePartX0 n range_
      maxPrime = (range_ - n) `div` 2 + 1
      ps = primesUpTo maxPrime
      results = map (\p -> let (gg, gs) = computePTorsionZWithGenerators p n range_
                           in (gg, (p, gs))) ps
      torsionParts = map fst results
      allGens = map snd results
  in (foldl (<>) freePart torsionParts, allGens)

-- | Integral cohomology of K(Z, n) via Universal Coefficients.
emCohomologyZ :: Int -> Int -> GradedGroup
emCohomologyZ n range_ = universalCoefficients (emHomologyZ n range_)

-- ---------------------------------------------------------------------------
-- Internal helpers for K(Z, n)
-- ---------------------------------------------------------------------------

-- | Free part of X(0) for K(Z, n).
--
-- * n even: P(σ^n u, n) has H_m = Z for every m that is a non-negative
--   multiple of n. This is the polynomial algebra on a generator of degree n.
-- * n odd: E(σ^n u, n) has H_m = Z for m ∈ {0, n} only.
--   This is the exterior algebra on a generator of degree n.
freePartX0 :: Int -> Int -> GradedGroup
freePartX0 n range_
  | even n =
      -- Polynomial algebra: Z in degree 0, n, 2n, 3n, ...
      GradedGroup $ IM.fromList
        [ (m, freeGroup 1) | m <- [0, n .. range_], m >= 0 ]
  | otherwise =
      -- Exterior algebra: Z in degree 0 and n only.
      GradedGroup $ IM.fromList $
        (0, freeGroup 1) : [ (n, freeGroup 1) | n <= range_ ]

-- | Compute the p-primary torsion of H_*(K(Z, n)) contributed by X''(p).
--
-- X''(p) is the tensor product of elementary complexes of type (II) built
-- from first-kind p-words. The generators are of the form
-- (σ^{k+1} γ_p α u, σ^k φ_p α u) for 0 ≤ k ≤ n-3, where α is a first-kind
-- p-word of height n-k-1.
--
-- The computation proceeds exactly as for K(Z/p^f, n), but:
--   * The base elementary complex is trivial (only Z in degree 0),
--     since there is no X'(p) for K(Z, n).
--   * All elementary complex factors have log-height 1 (order p).
computePTorsionZ :: Int -> Int -> Int -> GradedGroup
computePTorsionZ p n range_ = fst (computePTorsionZWithGenerators p n range_)

-- | Like 'computePTorsionZ' but also returns the admissible sequences used.
computePTorsionZWithGenerators :: Int -> Int -> Int -> (GradedGroup, [AdmissibleSeq])
computePTorsionZWithGenerators p n range_ =
  let base = GradedGroup (IM.singleton 0 (freeGroup 1))
      (result, gens) = foldl (\(gg, gs) sd ->
          let seqs = filter (\sq -> genus sq == 2) $
                     filterByEvenFirst (generateSequencesP p sd 0 (n - 1))
              ecDeg sq = (p - 1) * stableDegree sq + n
          in foldl (\(gg', gs') sq ->
               if stableDegree sq >= 0
               then ( kunneth gg' (ecHomology p (ecDeg sq) 1 range_)
                    , gs' ++ [sq] )
               else (gg', gs')
             ) (gg, gs) seqs
        ) (base, []) [0 .. (range_ - n) `div` max 1 (p - 1)]
  in (stripFree result, gens)

-- | Remove the free components from a graded group, keeping only torsion.
--
-- This removes all (0,0) entries (free rank) from each group in the grading.
stripFree :: GradedGroup -> GradedGroup
stripFree (GradedGroup m) = GradedGroup (IM.mapMaybe stripFreeG m)
  where
    stripFreeG (Group g) =
      let g' = Map.filterWithKey (\(pr, _) _ -> pr /= 0) g
      in if Map.null g' then Nothing else Just (Group g')

-- ---------------------------------------------------------------------------
-- Prime generation
-- ---------------------------------------------------------------------------

-- | Generate all primes up to a given bound using trial division.
--
-- >>> primesUpTo 20
-- [2, 3, 5, 7, 11, 13, 17, 19]
primesUpTo :: Int -> [Int]
primesUpTo n = [ p | p <- [2..n], isPrime p ]
  where
    isPrime k = k >= 2 && all (\d -> k `mod` d /= 0) [2..isqrt k]
    isqrt = floor . (sqrt :: Double -> Double) . fromIntegral
