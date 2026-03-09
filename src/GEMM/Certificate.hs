-- |
-- Module      : GEMM.Certificate
-- Description : Export proof certificates for Lean verification (Level 3).
--
-- This module generates a trace of the GEMM computation that can be
-- verified by the Lean 4 certificate checker in lean-lab/LeanLab/CertTypes.lean.
--
-- A certificate records:
--   * The base elementary complex.
--   * Each Künneth step with its admissible sequence generator.
--   * The final result.
--
-- The Lean verifier independently:
--   1. Checks each admissible sequence and recomputes the elementary complex.
--   2. Verifies completeness of the enumeration.
--   3. Recomputes each Künneth step.
--   4. Checks structural invariants.
module GEMM.Certificate
  ( Certificate(..)
  , CertStep(..)
  , computeCertificateP
  , certificateToLean
  ) where

import Control.DeepSeq (NFData(..))
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import GEMM.Types
import GEMM.GradedGroups (kunneth)
import GEMM.AdmissibleSequences
  ( AdmissibleSeq(..), stableDegree
  , generateSequencesP, filterByEvenFirst )
import GEMM.ElementaryComplex (ecHomology)

-- | A single Künneth step.
data CertStep = CertStep
  { csGenerator :: [Int]       -- ^ The p-admissible sequence that produced this factor.
  , csInput     :: GradedGroup -- ^ Accumulated group before this step.
  , csFactor    :: GradedGroup -- ^ Elementary complex factor.
  , csOutput    :: GradedGroup -- ^ Result after Künneth.
  } deriving (Show)

instance NFData CertStep where
  rnf (CertStep g i f o) = rnf g `seq` rnf i `seq` rnf f `seq` rnf o

-- | A full computation certificate.
data Certificate = Certificate
  { certPrime     :: Int
  , certExponent  :: Int
  , certDimension :: Int
  , certRange     :: Int
  , certBase      :: GradedGroup
  , certSteps     :: [CertStep]
  , certResult    :: GradedGroup
  } deriving (Show)

instance NFData Certificate where
  rnf (Certificate p e d r b s res) =
    rnf p `seq` rnf e `seq` rnf d `seq` rnf r
      `seq` rnf b `seq` rnf s `seq` rnf res

-- | Compute H_*(K(Z/p^f, n)) and produce a certificate.
computeCertificateP :: Int -> Int -> Int -> Int -> Certificate
computeCertificateP p f n range_ =
  let base = ecHomology p n f range_
      sds = [0 .. (range_ - n) `div` max 1 (p - 1)]
      -- Collect all (seq, degree) pairs that contribute
      allSeqs = concatMap (\sd ->
          let seqs = filterByEvenFirst (generateSequencesP p sd 0 (n - 1))
          in map (\sq -> (sq, (p - 1) * stableDegree sq + n)) seqs
        ) sds
      -- Build steps
      (finalGG, steps) = foldl (\(gg, acc) (sq, deg) ->
          if stableDegree sq >= 0
          then let factor = ecHomology p deg 1 range_
                   result = kunneth range_ gg factor
                   step = CertStep (getSeq sq) gg factor result
               in (result, acc ++ [step])
          else (gg, acc)
        ) (base, []) allSeqs
  in Certificate
       { certPrime     = p
       , certExponent  = f
       , certDimension = n
       , certRange     = range_
       , certBase      = base
       , certSteps     = steps
       , certResult    = finalGG
       }

-- | Render a certificate as Lean 4 source code.
certificateToLean :: Certificate -> String
certificateToLean cert = unlines $
  [ "-- Auto-generated GEMM certificate (Level 3)"
  , "-- K(Z/" ++ show (certPrime cert) ++ "^" ++ show (certExponent cert)
    ++ ", " ++ show (certDimension cert) ++ "), range " ++ show (certRange cert)
  , ""
  , "import LeanLab.CertTypes"
  , ""
  , "set_option linter.style.nativeDecide false"
  , ""
  , "def cert : GEMMCertificate :="
  , "  { prime := " ++ show (certPrime cert)
  , "  , exponent := " ++ show (certExponent cert)
  , "  , dimension := " ++ show (certDimension cert)
  , "  , computationRange := " ++ show (certRange cert)
  , "  , base :="
  ] ++ indentedGraded 6 (certBase cert) ++
  [ "  , steps :="
  ] ++ stepsToLean 6 (certSteps cert) ++
  [ "  , result :="
  ] ++ indentedGraded 6 (certResult cert) ++
  [ "  }"
  , ""
  , "-- Level 3 verification:"
  , "-- ✓ Base elementary complex is correct"
  , "-- ✓ Each generator is admissible and produces the correct factor"
  , "-- ✓ Enumeration of admissible sequences is complete"
  , "-- ✓ Each Künneth step is correct"
  , "-- ✓ Structural invariants hold"
  , "-- ✓ Proposition 4.1 verified via chain complex computation"
  , "example : verifyFull cert = true := by native_decide"
  ]

-- | Indent a string by n spaces.
indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s

-- | Render a GradedGroup as multiple indented Lean lines.
indentedGraded :: Int -> GradedGroup -> [String]
indentedGraded n (GradedGroup m) =
  case IM.toAscList m of
    [] -> [indent n "[]"]
    [(deg, grp)] ->
      [indent n $ "[(" ++ show deg ++ ", " ++ groupToLean grp ++ ")]"]
    (first:rest) ->
      [indent n $ "[ " ++ degToLean first]
      ++ map (\d -> indent n $ ", " ++ degToLean d) rest
      ++ [indent n "]"]
  where
    degToLean (deg, grp) = "(" ++ show deg ++ ", " ++ groupToLean grp ++ ")"

-- | Render a Group as Lean syntax (inline).
groupToLean :: Group -> String
groupToLean (Group m) =
  "[" ++ commaJoin (map compToLean (Map.toAscList m)) ++ "]"
  where
    compToLean ((p, k), power) =
      "⟨" ++ show p ++ ", " ++ show k ++ ", " ++ show power ++ "⟩"

-- | Render certificate steps as multiple indented Lean lines.
stepsToLean :: Int -> [CertStep] -> [String]
stepsToLean n [] = [indent n "[]"]
stepsToLean n steps =
  let rendered = zip ([0..] :: [Int]) (map stepToLines steps)
  in concatMap (\(i, ls) -> prefixStep n (i == 0) ls) rendered
     ++ [indent n "]"]
  where
    stepToLines (CertStep gen inp fac out) =
      let md = maxDegOf out
      in [ "{ generator := " ++ seqToLean gen
         , ", input :="
         ] ++ indentedGraded 4 inp ++
         [ ", factor :="
         ] ++ indentedGraded 4 fac ++
         [ ", output :="
         ] ++ indentedGraded 4 out ++
         [ ", maxDeg := " ++ show md ++ " }"
         ]
    prefixStep n' isFirst ls =
      case ls of
        [] -> []
        (l:rest) ->
          let pfx = if isFirst then "[ " else ", "
          in indent n' (pfx ++ l) : map (indent (n' + 2)) rest
    maxDegOf (GradedGroup m)
      | IM.null m = 0
      | otherwise = fst (IM.findMax m)

-- | Render an admissible sequence as Lean list syntax.
seqToLean :: [Int] -> String
seqToLean xs = "[" ++ commaJoin (map show xs) ++ "]"

-- | Join strings with ", ".
commaJoin :: [String] -> String
commaJoin [] = ""
commaJoin [x] = x
commaJoin (x:xs) = x ++ ", " ++ commaJoin xs
