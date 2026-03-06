-- |
-- Module      : EMM.AdmissibleSequences
-- Description : Generation and manipulation of admissible sequences.
--
-- An admissible sequence (in the sense of Steenrod/Cartan) is a finite
-- sequence of non-negative integers @(a_1, a_2, ..., a_k)@ satisfying
-- the admissibility condition @a_i >= p * a_{i+1}@ for all i, where
-- p is the prime under consideration.
--
-- For p = 2, this recovers the classical mod-2 admissible sequences
-- used in the Steenrod algebra: @a_i >= 2 * a_{i+1}@.
--
-- == Key invariants
--
--   * Stable degree: @sum a_i@ (the total degree of the corresponding
--     Steenrod operation).
--   * Excess: @p * a_1 - (p-1) * stableDegree@. For p=2 this simplifies
--     to @2 * a_1 - stableDegree@.
--   * Genus: classifies sequences into 3 types based on @a_1@ and the
--     parity of the last element.
--
-- == References
--
-- Cartan, H. — Algèbres d'Eilenberg-MacLane, Séminaire H. Cartan, 1954-55.
-- Clément, A. — Integral Cohomology of Finite Postnikov Towers, Appendix B.
-- Tischler, N. — Chapter II, §II.1-II.2.
module EMM.AdmissibleSequences
  ( AdmissibleSeq(..)
  , admissible
  , admissibleForPrime
  , stableDegree
  , excess
  , excessForPrime
  , genus
  , firstEntry
  , lastEntry
  , generateSequences
  , generateSequencesP
  , filterByEvenFirst
  , filterByHeight
  , firstCech
  ) where

-- | An admissible sequence is a list of non-negative integers satisfying
-- the admissibility condition a_i >= p * a_{i+1} for the relevant prime p.
newtype AdmissibleSeq = AdmissibleSeq { getSeq :: [Int] }
  deriving (Eq, Show)

-- | Check admissibility for p = 2: @a_i >= 2 * a_{i+1}@ for all i.
admissible :: AdmissibleSeq -> Bool
admissible = admissibleForPrime 2

-- | Check admissibility for a given prime p: @a_i >= p * a_{i+1}@ for all i.
admissibleForPrime :: Int -> AdmissibleSeq -> Bool
admissibleForPrime p (AdmissibleSeq xs) = go xs
  where
    go (a:b:rest) = a >= p * b && go (b:rest)
    go _          = True

-- | Stable degree of a sequence: the sum of its elements.
--
-- By convention, the empty sequence has stable degree -1 (matching the
-- C++ implementation). This convention is critical: it prevents spurious
-- Künneth factors when processing the empty sequence.
stableDegree :: AdmissibleSeq -> Int
stableDegree (AdmissibleSeq []) = -1
stableDegree (AdmissibleSeq xs) = sum xs

-- | Excess for p = 2: @2 * a_1 - stableDegree@.
excess :: AdmissibleSeq -> Int
excess = excessForPrime 2

-- | Excess for a given prime p.
--
-- For a sequence @(a_1, ..., a_k)@:
-- @excess_p = p * a_1 - (p - 1) * stableDegree@
--
-- This measures how far the sequence is from being "tight".
-- For p = 2 it simplifies to the classical @2 * a_1 - sum a_i@.
excessForPrime :: Int -> AdmissibleSeq -> Int
excessForPrime p s@(AdmissibleSeq xs) = p * head' xs - (p - 1) * stableDegree s
  where head' []    = 0
        head' (a:_) = a

-- | First element of the sequence, or -1 if empty.
firstEntry :: AdmissibleSeq -> Int
firstEntry (AdmissibleSeq [])    = -1
firstEntry (AdmissibleSeq (x:_)) = x

-- | Last element of the sequence, or -1 if empty.
lastEntry :: AdmissibleSeq -> Int
lastEntry (AdmissibleSeq [])  = -1
lastEntry (AdmissibleSeq xs)  = last xs

-- | Genus of an admissible sequence.
--
--   * Genus 1: @a_1 ∈ {0, 1}@ (degenerate case).
--   * Genus 2: last element is even → first-kind p-word (X''(p) in Tischler).
--   * Genus 3: last element is odd → second-kind p-word (X'''(p) in Tischler).
genus :: AdmissibleSeq -> Int
genus s
  | a0 == 0 || a0 == 1       = 1
  | even (lastEntry s)        = 2
  | otherwise                 = 3
  where a0 = firstEntry s

-- | Generate all admissible sequences (for p = 2) with given stable degree
-- and excess in @[minE..maxE]@.
generateSequences :: Int -> Int -> Int -> [AdmissibleSeq]
generateSequences = generateSequencesP 2

-- | Generate all admissible sequences for a given prime p with given
-- stable degree and excess in @[minE..maxE]@.
--
-- Uses a recursive backtracking algorithm: at each step, the first element
-- @a@ of the remaining sequence is determined by @a = (sd + e) / p@ (for
-- general p) or @a = (sd + e) / 2@ (for p = 2), and we recurse on the
-- remaining stable degree and all valid excess values.
--
-- The algorithm is a direct port of the C++ @iterate()@ function from
-- @Mod2AdmissibleSequences.cpp@, generalized to arbitrary primes.
generateSequencesP :: Int -> Int -> Int -> Int -> [AdmissibleSeq]
generateSequencesP p sd minE maxE =
  concatMap (\e -> if (sd * (p - 1) + e) `mod` p == 0
                   then iterateP p (AdmissibleSeq []) sd e
                   else [])
            [minE..maxE]

-- | Recursive generation of admissible sequences for prime p.
iterateP :: Int -> AdmissibleSeq -> Int -> Int -> [AdmissibleSeq]
iterateP p incoming sd e
  | (sd * (p - 1) + e) `mod` p /= 0 = []
  | a > sd                           = []
  | sd == 0 && e == 0                = [incoming]
  | otherwise =
      let newSD = sd - a
      in concatMap (\i ->
           let newE = p * i - (p - 1) * newSD
           in if newE >= 0
              then iterateP p (appendSeq incoming a) newSD newE
              else []
         ) [a `div` p - (e `div` p) .. a `div` p]
  where
    a = (sd * (p - 1) + e) `div` p

-- | Append an element to the end of a sequence.
appendSeq :: AdmissibleSeq -> Int -> AdmissibleSeq
appendSeq (AdmissibleSeq xs) v = AdmissibleSeq (xs ++ [v])

-- | Keep only sequences whose first element is even.
--
-- For p = 2, this filters to sequences whose associated p-word has
-- even-degree generator (needed for the Eilenberg-MacLane algorithm).
filterByEvenFirst :: [AdmissibleSeq] -> [AdmissibleSeq]
filterByEvenFirst = filter (\s -> case getSeq s of
                                    []    -> True
                                    (x:_) -> even x)

-- | Remove sequences with even first element and excess equal to the
-- given height bound. Used to avoid redundant generators.
filterByHeight :: Int -> [AdmissibleSeq] -> [AdmissibleSeq]
filterByHeight height = filter (\s ->
  not (even (firstEntry s) && excess s == height))

-- | Remove the first element of a sequence (first Čech operation).
firstCech :: AdmissibleSeq -> AdmissibleSeq
firstCech (AdmissibleSeq [])     = AdmissibleSeq []
firstCech (AdmissibleSeq (_:xs)) = AdmissibleSeq xs
