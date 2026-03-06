-- |
-- Module      : EMM.JSON
-- Description : JSON rendering of EMM results.
--
-- Generates JSON output for homology, cohomology, and generators
-- of Eilenberg-MacLane spaces. No external dependencies required.
module EMM.JSON
  ( renderJsonP
  , renderJsonZ
  ) where

import Prelude hiding (Word)
import Data.List (intercalate)
import qualified Data.IntMap.Strict as IM

import EMM.Types
import EMM.AdmissibleSequences
import EMM.Words
import EMM.LaTeX (ecType, renderGroup)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy as TL

-- | Render JSON for K(Z/p^f, n).
renderJsonP :: Int -> Int -> Int -> GradedGroup -> GradedGroup -> [AdmissibleSeq] -> String
renderJsonP p f n homology cohomology gens =
  let ac = max (anticonnexity homology) (anticonnexity cohomology)
  in unlines
    [ "{"
    , "  \"space\": " ++ jsonStr (spaceNameP p f n) ++ ","
    , "  \"parameters\": { \"p\": " ++ show p
                      ++ ", \"f\": " ++ show f
                      ++ ", \"n\": " ++ show n
                      ++ ", \"range\": " ++ show ac ++ " },"
    , "  \"homology\": " ++ renderGradedGroupJson homology ac ++ ","
    , "  \"cohomology\": " ++ renderGradedGroupJson cohomology ac ++ ","
    , "  \"generators\": " ++ renderGeneratorsJsonP p n f gens
    , "}"
    ]

-- | Render JSON for K(Z, n).
renderJsonZ :: Int -> GradedGroup -> GradedGroup -> [(Int, [AdmissibleSeq])] -> String
renderJsonZ n homology cohomology primeGens =
  let ac = max (anticonnexity homology) (anticonnexity cohomology)
  in unlines
    [ "{"
    , "  \"space\": " ++ jsonStr ("K(\\Z," ++ show n ++ ")") ++ ","
    , "  \"parameters\": { \"n\": " ++ show n
                      ++ ", \"range\": " ++ show ac ++ " },"
    , "  \"homology\": " ++ renderGradedGroupJson homology ac ++ ","
    , "  \"cohomology\": " ++ renderGradedGroupJson cohomology ac ++ ","
    , "  \"generators\": " ++ renderGeneratorsJsonZ n primeGens
    , "}"
    ]

-- ---------------------------------------------------------------------------
-- Graded group rendering
-- ---------------------------------------------------------------------------

renderGradedGroupJson :: GradedGroup -> Int -> String
renderGradedGroupJson gg ac =
  "{\n" ++ intercalate ",\n"
    [ "    " ++ jsonStr (show deg) ++ ": "
      ++ jsonStr (groupToLatex (groupInDegree deg gg))
    | deg <- [0..ac]
    ] ++ "\n  }"

-- | Convert a Group to its LaTeX representation, reusing EMM.LaTeX.renderGroup.
groupToLatex :: Group -> String
groupToLatex = TL.unpack . toLazyText . renderGroup

-- ---------------------------------------------------------------------------
-- Generator rendering for K(Z/p^f, n)
-- ---------------------------------------------------------------------------

renderGeneratorsJsonP :: Int -> Int -> Int -> [AdmissibleSeq] -> String
renderGeneratorsJsonP p n f gens =
  "[\n" ++ intercalate ",\n" (map renderOne gens) ++ "\n  ]"
  where
    oneSeq = AdmissibleSeq [1]

    renderOne sq =
      let sd = stableDegree sq
          deg = (p - 1) * sd + n
          f' = if sd == 0 then f else 1
          w  = gFunction p sq n f'
          w2 = if sd == 0
               then gFunction p oneSeq n f
               else wordFirstCech w
          f2 = if sd == 0 then f else 1
      in "    { \"degree\": " ++ show deg
         ++ ", \"genus\": " ++ show (genus sq)
         ++ ", \"type\": " ++ jsonStr (ecTypeStr deg)
         ++ ", \"sequence\": " ++ renderSeqJson sq
         ++ ", \"pair\": [" ++ jsonStr (wordToStr p w f')
         ++ ", " ++ jsonStr (wordToStr p w2 f2) ++ "] }"

-- ---------------------------------------------------------------------------
-- Generator rendering for K(Z, n)
-- ---------------------------------------------------------------------------

renderGeneratorsJsonZ :: Int -> [(Int, [AdmissibleSeq])] -> String
renderGeneratorsJsonZ n primeGens =
  "[\n" ++ intercalate ",\n" (x0Row : concatMap renderPrimeGroup primeGens) ++ "\n  ]"
  where
    x0Row = "    { \"prime\": null"
         ++ ", \"degree\": " ++ show n
         ++ ", \"genus\": 1"
         ++ ", \"type\": " ++ jsonStr (if even n then "P" else "E")
         ++ ", \"generator\": " ++ jsonStr ("σ^" ++ show n) ++ " }"

    renderPrimeGroup (_, []) = []
    renderPrimeGroup (p, seqs) = map (renderOneZ p) seqs

    renderOneZ p sq =
      let sd = stableDegree sq
          deg = (p - 1) * sd + n
          w  = gFunction p sq n 1
      in "    { \"prime\": " ++ show p
         ++ ", \"degree\": " ++ show deg
         ++ ", \"genus\": " ++ show (genus sq)
         ++ ", \"type\": " ++ jsonStr (ecTypeStr deg)
         ++ ", \"sequence\": " ++ renderSeqJson sq
         ++ ", \"pair\": [" ++ jsonStr (wordToStr p w 1)
         ++ ", " ++ jsonStr (wordToStr p (wordFirstCech w) 1) ++ "] }"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | JSON-escape a string.
jsonStr :: String -> String
jsonStr s = "\"" ++ concatMap escChar s ++ "\""
  where
    escChar '"'  = "\\\""
    escChar '\\' = "\\\\"
    escChar c    = [c]

-- | Render an admissible sequence as a JSON array.
renderSeqJson :: AdmissibleSeq -> String
renderSeqJson (AdmissibleSeq xs) = "[" ++ intercalate ", " (map show xs) ++ "]"

-- | Convert ecType Builder to String.
ecTypeStr :: Int -> String
ecTypeStr d = TL.unpack (toLazyText (ecType d))

-- | Space name for K(Z/p^f, n) in LaTeX.
spaceNameP :: Int -> Int -> Int -> String
spaceNameP p f n
  | f == 1    = "K(\\Z/" ++ show p ++ "," ++ show n ++ ")"
  | otherwise = "K(\\Z/" ++ show p ++ "^{" ++ show f ++ "}," ++ show n ++ ")"

-- | Render a word as a plain text string.
wordToStr :: Int -> Word -> Int -> String
wordToStr _ (Word []) _ = ""
wordToStr p (Word pls) f = concatMap renderPL pls
  where
    renderPL (PL BetaP pw)  = "β_" ++ show p ++ showPow pw
    renderPL (PL Sigma pw)  = "σ" ++ showPow pw
    renderPL (PL GammaP pw) = "γ_" ++ show p ++ showPow pw
    renderPL (PL PhiP pw)   = "φ_" ++ show p ++ showPow pw
    renderPL (PL PsiPF pw)  =
      (if f == 1 then "ψ_" ++ show p
       else "ψ_{" ++ show p ++ "^" ++ show f ++ "}")
      ++ showPow pw
    showPow 1 = ""
    showPow pw = "^" ++ show pw
