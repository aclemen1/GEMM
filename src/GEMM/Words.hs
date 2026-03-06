-- |
-- Module      : GEMM.Words
-- Description : Symbolic words representing Steenrod operations.
--
-- A p-word (Tischler, Def. 2.5) is a finite sequence of letters from the
-- alphabet {σ, γ_p, φ_p}, optionally terminated by ψ_{p,f}.
--
-- == Letters and their degree formulas
--
--   * σ (Sigma):   @deg(σα) = 1 + deg α@ — suspension.
--   * γ_p (GammaP): @deg(γ_p α) = p · deg α@ — divided power.
--   * φ_p (PhiP):   @deg(φ_p α) = 2 + p · deg α@ — Cartan operation.
--   * β_p (BetaP):  @deg(β_p α) = 1 + deg α@ — Bockstein mod p.
--   * ψ_{p,f} (PsiPF): degree 2, height 1 — initial symbol for p^f-torsion.
--
-- For p = 2, the letters β_2, σ, γ_2, φ_2, ψ_{2,s} recover the original
-- EMM alphabet.
--
-- == The g-function
--
-- The g-function converts an admissible sequence into a word of Steenrod
-- operations. It is parameterized by the prime p and implements the recursive
-- algorithm from Clément's thesis (Appendix B), generalized to arbitrary primes.
--
-- == References
--
-- Clément, A. — Integral Cohomology of Finite Postnikov Towers, Appendix B.
-- Tischler, N. — Chapter II, Def. 2.5.
module GEMM.Words
  ( Letter(..)
  , PoweredLetter(..)
  , Word(..)
  , concatLetter
  , gFunction
  , wordFirstCech
  ) where

import Prelude hiding (Word)
import GEMM.AdmissibleSequences

-- | Letters of the p-word alphabet.
--
-- The prime p is not stored in the letter itself but is a parameter
-- of the algorithms that use these letters (e.g. degree computation,
-- g-function, LaTeX rendering).
data Letter
  = BetaP    -- ^ β_p: Bockstein mod p, degree +1
  | Sigma    -- ^ σ: suspension, degree +1
  | GammaP   -- ^ γ_p: divided power, degree ×p
  | PhiP     -- ^ φ_p: Cartan operation, degree ×p + 2
  | PsiPF    -- ^ ψ_{p,f}: initial symbol for p^f-torsion, degree 2
  deriving (Eq, Show)

-- | A letter raised to a power (for compact representation).
-- @PL letter k@ means the letter applied k times.
data PoweredLetter = PL !Letter !Int
  deriving (Eq, Show)

-- | A word is a sequence of powered letters.
-- Adjacent identical letters are fused (e.g. σ² instead of σσ).
newtype Word = Word { getWord :: [PoweredLetter] }
  deriving (Eq, Show)

-- | The empty word (identity operation).
emptyWord :: Word
emptyWord = Word []

-- | Append a letter (with given power) to a word, fusing with the last
-- letter if they match.
--
-- >>> concatLetter Sigma 2 (Word [PL Sigma 3])
-- Word [PL Sigma 5]
concatLetter :: Letter -> Int -> Word -> Word
concatLetter _ 0 w = w
concatLetter l p (Word []) = Word [PL l p]
concatLetter l p (Word pls)
  | PL l' p' <- last pls, l' == l = Word (init pls ++ [PL l (p' + p)])
  | otherwise = Word (pls ++ [PL l p])

-- | Convert an admissible sequence into a word of Steenrod operations.
--
-- @gFunction p seq n s@ converts the admissible sequence @seq@ into a
-- p-word, given connectivity @n@ and log-order @s@.
--
-- The algorithm recurses on the sequence, consuming one entry at each step.
-- There are 5 cases based on the first entry a_0 and the excess:
--
--   1. @a_0 <= 0@: emit σ^n (pure suspension).
--   2. @a_0 == 1@: emit ψ_{p,f} preceded by σ^{n-1}.
--   3. @a_0 even, excess < n@: emit β_p σ^{n-e-1} φ_p, recurse on Čech.
--   4. @a_0 even, excess == n@: emit γ_p, recurse.
--   5. @a_0 odd@: emit σ^{n-e} φ_p, recurse.
--
-- This is a direct generalization of the C++ g_function. For p = 2,
-- it produces the same words as the original implementation.
gFunction :: Int -> AdmissibleSeq -> Int -> Int -> Word
gFunction _p seq0 n _s = go seq0 n emptyWord
  where
    go s nn w =
      let a0 = firstEntry s
          e  = excess s
      in
        if a0 <= 0 then
          -- Case 1: degenerate — pure suspension
          concatLetter Sigma nn w
        else if a0 == 1 then
          -- Case 2: ψ_{p,f} preceded by suspension
          let w' = if nn >= 2 then concatLetter Sigma (nn - 1) w else w
          in concatLetter PsiPF 1 w'
        else if even a0 && e < nn then
          -- Case 3: β_p σ^{n-e-1} φ_p, then recurse
          let w1 = concatLetter BetaP 1 w
              w2 = if nn - e - 1 > 0
                   then concatLetter Sigma (nn - e - 1) w1
                   else w1
              w3 = concatLetter PhiP 1 w2
          in go (firstCech s) e w3
        else if even a0 && e == nn then
          -- Case 4: γ_p, then recurse
          let w1 = concatLetter GammaP 1 w
          in go (firstCech s) nn w1
        else
          -- Case 5: odd a0 — σ^{n-e} φ_p, then recurse
          let w1 = if nn - e > 0
                   then concatLetter Sigma (nn - e) w
                   else w
              w2 = concatLetter PhiP 1 w1
          in go (firstCech s) (e - 1) w2

-- | Remove the first letter from a word (Čech on words).
wordFirstCech :: Word -> Word
wordFirstCech (Word [])     = emptyWord
wordFirstCech (Word (_:xs)) = Word xs
