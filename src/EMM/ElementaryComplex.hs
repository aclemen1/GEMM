module EMM.ElementaryComplex
  ( nu2
  , ecHomology
  , ecCohomology
  ) where

import qualified Data.IntMap.Strict as IM
import EMM.Types
import EMM.GradedGroups (universalCoefficients)

-- | 2-adic valuation
nu2 :: Int -> Int
nu2 0 = error "nu2: zero"
nu2 a = go (abs a) 0
  where
    go x n
      | even x    = go (x `div` 2) (n + 1)
      | otherwise  = n

-- | Homology of elementary complex with given degree, log-height, and anticonnexity bound.
ecHomology :: Int -> Int -> Int -> GradedGroup
ecHomology degree logh ac =
  let z0 = GradedGroup (IM.singleton 0 (singletonGroup 0 1))
      cells = IM.fromListWith (<>)
        [ (m, singletonGroup logord 1)
        | m <- [1..ac]
        , validDegree degree m
        , let logord = computeLogorder degree logh m
        ]
  in z0 <> GradedGroup cells
  where
    validDegree deg m
      | even deg  = m `mod` deg == 0
      | otherwise = (m + 1) `mod` (deg + 1) == 0

    computeLogorder deg lh m
      | even deg  = lh + nu2 (m `div` deg)
      | otherwise = lh

ecCohomology :: Int -> Int -> Int -> GradedGroup
ecCohomology degree logh ac = universalCoefficients (ecHomology degree logh ac)
