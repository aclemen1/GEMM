module EMM.EilenbergMacLane
  ( emHomology
  , emHomologyWithGenerators
  , emCohomology
  ) where

import EMM.Types (GradedGroup)
import EMM.GradedGroups (kunneth, universalCoefficients)
import EMM.AdmissibleSequences
import EMM.ElementaryComplex (ecHomology)

-- | Compute the integral homology of K(Z/2^s, n) up to given range.
emHomology :: Int -> Int -> Int -> GradedGroup
emHomology s n range_ = fst (emHomologyWithGenerators s n range_)

-- | Compute homology and collect the admissible sequences (generators) used.
emHomologyWithGenerators :: Int -> Int -> Int -> (GradedGroup, [AdmissibleSeq])
emHomologyWithGenerators s n range_ =
  let base = ecHomology n s range_
      sdZero = AdmissibleSeq [0]
      (result, gens) = foldl (\(gg, gs) sd ->
          let seqs = filterByEvenFirst (generateSequences sd 0 (n - 1))
          in foldl (\(gg', gs') sq ->
               if stableDegree sq >= 0
               then ( kunneth gg' (ecHomology (stableDegree sq + n) 1 range_)
                    , gs' ++ [sq] )
               else (gg', gs')
             ) (gg, gs) seqs
        ) (base, [sdZero]) [0..range_ - n]
  in (result, gens)

emCohomology :: Int -> Int -> Int -> GradedGroup
emCohomology s n range_ = universalCoefficients (emHomology s n range_)
