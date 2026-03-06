-- |
-- Module      : GEMM.LaTeX
-- Description : LaTeX rendering of EMM results.
--
-- Generates LaTeX documents displaying the integral homology and cohomology
-- groups of Eilenberg-MacLane spaces, along with the generators (admissible
-- sequences and their associated Steenrod words) involved in the computation.
--
-- The output uses the @supertabular@ package for multi-page tables.
module GEMM.LaTeX
  ( renderDocument
  , renderDocumentP
  , renderDocumentZ
  , renderTopMatter
  , renderBackMatter
  , renderTwoColumns
  , renderGenerators
  , renderGroup
  , renderWord
  , ecType
  ) where

import Prelude hiding (Word)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import qualified Data.Map.Strict as Map

import GEMM.Types
import GEMM.AdmissibleSequences
import GEMM.Words

-- | Render a complete LaTeX document for K(Z/2^s, n).
-- Backward-compatible wrapper around 'renderDocumentP'.
renderDocument :: Int -> Int -> GradedGroup -> GradedGroup -> [AdmissibleSeq] -> Text
renderDocument s n = renderDocumentP 2 s n

-- | Render a complete LaTeX document for K(Z/p^f, n).
--
-- @renderDocumentP p f n homology cohomology generators@
renderDocumentP :: Int -> Int -> Int -> GradedGroup -> GradedGroup -> [AdmissibleSeq] -> Text
renderDocumentP p f n homology cohomology gens = toLazyText $
  renderTopMatter
  <> str ("\\subsection*{Homology and cohomology groups of $K(\\Z/"
          ++ show p ++ "^{" ++ show f ++ "}," ++ show n ++ ")$.}")
  <> renderTwoColumns homology cohomology
  <> str "\\newpage"
  <> str "\\subsection*{Generators involved in the calculus.}"
  <> renderGenerators p gens n f
  <> renderBackMatter

-- | Render a complete LaTeX document for K(Z, n).
renderDocumentZ :: Int -> GradedGroup -> GradedGroup -> [(Int, [AdmissibleSeq])] -> Text
renderDocumentZ n homology cohomology primeGens = toLazyText $
  renderTopMatter
  <> str ("\\subsection*{Homology and cohomology groups of $K(\\Z," ++ show n ++ ")$.}")
  <> renderTwoColumns homology cohomology
  <> str "\\newpage"
  <> str "\\subsection*{Generators involved in the calculus.}"
  <> renderGeneratorsZ primeGens n
  <> renderBackMatter

-- | LaTeX preamble.
renderTopMatter :: Builder
renderTopMatter = mconcat
  [ str "\\documentclass[12pt,a4paper]{article}\n"
  , str "\\usepackage{amsfonts}\n"
  , str "\\usepackage{supertabular}\n"
  , str "\\begin{document}\n"
  , str "\\newcommand{\\Z}{\\mathbb{Z}}\n"
  , str "\\renewcommand{\\thefootnote}{\\fnsymbol{footnote}}"
  , str "\\section*{The Generalized Eilenberg-MacLane Machine\n"
  , str "\\footnote{\\tiny Alain Cl\\'ement, Ph.D. Thesis, Institute of Mathematics, University of Lausanne, Switzerland.}\n"
  , str "}\n"
  , str "{\\it A Haskell program designed to compute the integral homology and cohomology groups"
  , str " of Eilenberg-MacLane spaces $K(\\Z/p^f,n)$ for any prime $p$, and $K(\\Z,n)$.}\\\\"
  ]

-- | LaTeX document ending.
renderBackMatter :: Builder
renderBackMatter = str "\\end{document}\n"

-- | Render a two-column table of homology and cohomology groups.
renderTwoColumns :: GradedGroup -> GradedGroup -> Builder
renderTwoColumns gg1 gg2 =
  let ac = max (anticonnexity gg1) (anticonnexity gg2)
  in mconcat
    [ str "\\tablehead{\\hline%\n"
    , str "\t$n$ &$H_n(-,\\Z)$ &$H^n(-,\\Z)$\\\\%\n"
    , str "\t\\hline &&\\\\}\n"
    , str "\\tabletail{\\hline%\n"
    , str "\t\\multicolumn{3}{r}{%\n"
    , str "\t\\small\\slshape to be continued on the next page}\\\\}\n"
    , str "\\tablelasttail{\\hline}\n"
    , str "\\begin{supertabular}{|c|p{5.5cm}|p{5.5cm}|}\n"
    , mconcat [ renderRow n gg1 gg2 | n <- [0..ac] ]
    , str "&&\\\\"
    , str "\\end{supertabular}\n"
    ]

-- | Render one row of the homology/cohomology table.
renderRow :: Int -> GradedGroup -> GradedGroup -> Builder
renderRow n gg1 gg2 = mconcat
  [ str "$", str (show n), str "$%\n"
  , str "&$", renderGroup (groupInDegree n gg1), str "$%\n"
  , str "&$", renderGroup (groupInDegree n gg2), str "$\\\\\n\n"
  ]

-- | Render a 'Group' as a LaTeX expression.
--
-- Examples: @\\Z@, @\\Z/3^{2}@, @(\\Z/2)^{\\oplus 5}@, @\\Z\\oplus\\Z/2@.
renderGroup :: Group -> Builder
renderGroup (Group m)
  | Map.null m = str "(0)"
  | otherwise = mconcat $ interleave (str "\\oplus") $ map renderComp (Map.toAscList m)
  where
    -- Free part
    renderComp ((0, 0), r)
      | r > 1     = str "\\Z" <> str "^{\\oplus" <> str (show r) <> str "}"
      | otherwise = str "\\Z"
    -- Torsion part: Z/p^k or (Z/p^k)^m
    renderComp ((p, k), power) =
      let base = str ("\\Z/" ++ show p)
                 <> (if k > 1 then str "^{" <> str (show k) <> str "}" else mempty)
          paren = power > 1
          wrapped = if paren then str "(" <> base <> str ")" else base
      in wrapped <> (if power > 1 then str "^{\\oplus" <> str (show power) <> str "}" else mempty)
    interleave _ []     = []
    interleave _ [x]    = [x]
    interleave sep (x:xs) = x : sep : interleave sep xs

-- | Render the table of generators (admissible sequences and their Steenrod words).
--
-- @renderGenerators p gens n f@ where p is the prime, gens are the sequences,
-- n is the connectivity, and f is the log-order.
renderGenerators :: Int -> [AdmissibleSeq] -> Int -> Int -> Builder
renderGenerators p gens n f = mconcat
  [ str "\\tablehead{\\hline%\n"
  , str "\tDegree &Genus &Type &Generator\\\\%\n"
  , str "\t\\hline &&&\\\\}\n"
  , str "\\tabletail{\\hline%\n"
  , str "\t\\multicolumn{4}{r}{%\n"
  , str "\t\\small\\slshape to be continued on the next page}\\\\}\n"
  , str "\\tablelasttail{\\hline}\n"
  , str "\\begin{supertabular}{|c|c|c|p{8cm}|}\n"
  , mconcat (map renderGenRow gens)
  , str "&&&\\\\"
  , str "\\end{supertabular}\n"
  ]
  where
    oneSeq = AdmissibleSeq [1]

    renderGenRow sq =
      let sd = stableDegree sq
          deg = (p - 1) * sd + n
          f' = if sd == 0 then f else 1
          w  = gFunction p sq n f'
          secondWord = if sd == 0
                       then renderWord p (gFunction p oneSeq n f) f
                       else renderWord p (wordFirstCech w) 1
      in mconcat
        [ str "$", str (show deg), str "$\n"
        , str "&$", str (show (genus sq)), str "$\n"
        , str "&$", ecType deg, str "$"
        , str " &$("
        , renderWord p w f'
        , str ","
        , secondWord
        , str ")$\\\\\n"
        ]

-- | Render a Steenrod word as a LaTeX expression.
--
-- @renderWord p word f@ where p is the prime and f is the log-order
-- (used to render ψ_{p^f}).
renderWord :: Int -> Word -> Int -> Builder
renderWord _ (Word []) _ = mempty
renderWord p (Word pls) f = mconcat (map renderPL pls)
  where
    renderPL (PL BetaP pw)  = str ("\\beta_" ++ show p) <> showPow pw
    renderPL (PL Sigma pw)  = str "\\sigma" <> showPow pw
    renderPL (PL GammaP pw) = str ("\\gamma_" ++ show p) <> showPow pw
    renderPL (PL PhiP pw)   = str ("\\varphi_" ++ show p) <> showPow pw
    renderPL (PL PsiPF pw)  =
      (if f == 1
       then str ("\\psi_" ++ show p)
       else str ("\\psi_{" ++ show p ++ "^{" ++ show f ++ "}}"))
      <> showPow pw
    showPow 1 = mempty
    showPow pw = str "^{" <> str (show pw) <> str "}"

-- | Render the table of generators for K(Z, n), grouped by prime.
--
-- Includes the free part generator X(0) as the first row:
--   * n even: P(σⁿu, n) — polynomial algebra (genus 1).
--   * n odd:  E(σⁿu, n) — exterior algebra (genus 1).
renderGeneratorsZ :: [(Int, [AdmissibleSeq])] -> Int -> Builder
renderGeneratorsZ primeGens n = mconcat
  [ str "\\tablehead{\\hline%\n"
  , str "\tPrime &Degree &Genus &Type &Generator\\\\%\n"
  , str "\t\\hline &&&&\\\\}\n"
  , str "\\tabletail{\\hline%\n"
  , str "\t\\multicolumn{5}{r}{%\n"
  , str "\t\\small\\slshape to be continued on the next page}\\\\}\n"
  , str "\\tablelasttail{\\hline}\n"
  , str "\\begin{supertabular}{|c|c|c|c|p{7cm}|}\n"
  , renderX0Row
  , mconcat (concatMap renderPrimeGroup primeGens)
  , str "&&&&\\\\"
  , str "\\end{supertabular}\n"
  ]
  where
    -- X(0): free part generator σⁿ.
    renderX0Row = mconcat
      [ str "$-$\n"
      , str "&$", str (show n), str "$\n"
      , str "&$1$\n"
      , str "&$", str (if even n then "P" else "E"), str "$"
      , str " &$\\sigma^{", str (show n), str "}$\\\\\n"
      ]

    renderPrimeGroup (_, []) = []
    renderPrimeGroup (p, seqs) = map (renderGenRowZ p) seqs

    renderGenRowZ p sq =
      let sd = stableDegree sq
          deg = (p - 1) * sd + n
          w  = gFunction p sq n 1
      in mconcat
        [ str "$", str (show p), str "$\n"
        , str "&$", str (show deg), str "$\n"
        , str "&$", str (show (genus sq)), str "$\n"
        , str "&$", ecType deg, str "$"
        , str " &$("
        , renderWord p w 1
        , str ","
        , renderWord p (wordFirstCech w) 1
        , str ")$\\\\\n"
        ]

-- | Elementary complex type based on the degree of the first generator.
--
-- * Even degree: @P \\otimes E@ — polynomial on x, exterior on y.
-- * Odd degree: @E \\otimes P@ — exterior on x, polynomial on y.
ecType :: Int -> Builder
ecType d
  | even d    = str "P \\otimes E"
  | otherwise = str "E \\otimes P"

-- | Helper to convert a String to a Builder.
str :: String -> Builder
str = fromString
