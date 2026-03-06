module EMM.LaTeX
  ( renderDocument
  , renderTopMatter
  , renderBackMatter
  , renderTwoColumns
  , renderGenerators
  , renderGroup
  , renderWord
  ) where

import Prelude hiding (Word)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import qualified Data.IntMap.Strict as IM

import EMM.Types
import EMM.AdmissibleSequences
import EMM.Words

renderDocument :: Int -> Int -> GradedGroup -> GradedGroup -> [AdmissibleSeq] -> Text
renderDocument s n homology cohomology gens = toLazyText $
  renderTopMatter
  <> str ("\\subsection*{Homology and cohomology groups of $K(\\Z/2^{" ++ show s ++ "}," ++ show n ++ ")$.}")
  <> renderTwoColumns homology cohomology
  <> str "\\newpage"
  <> str "\\subsection*{Generators involved in the calculus.}"
  <> renderGenerators gens n s
  <> renderBackMatter

renderTopMatter :: Builder
renderTopMatter = mconcat
  [ str "\\documentclass[12pt,a4paper]{article}\n"
  , str "\\usepackage{amsfonts}\n"
  , str "\\usepackage{supertabular}\n"
  , str "\\begin{document}\n"
  , str "\\newcommand{\\Z}{\\mathbb{Z}}\n"
  , str "\\renewcommand{\\thefootnote}{\\fnsymbol{footnote}}"
  , str "\\section*{The Eilenberg-MacLane machine\n"
  , str "\\footnote{\\tiny Alain Cl\\'ement, Ph.D. Thesis, Institute of Mathematics, University of Lausanne, Switzerland.}\n"
  , str "}\n"
  , str "{\\it A Haskell program designed to compute the integral homology and cohomology groups"
  , str " of Eilenberg-MacLane spaces.}\\\\"
  ]

renderBackMatter :: Builder
renderBackMatter = str "\\end{document}\n"

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

renderRow :: Int -> GradedGroup -> GradedGroup -> Builder
renderRow n gg1 gg2 = mconcat
  [ str "$", str (show n), str "$%\n"
  , str "&$", renderGroup (groupInDegree n gg1), str "$%\n"
  , str "&$", renderGroup (groupInDegree n gg2), str "$\\\\\n\n"
  ]

renderGroup :: Group -> Builder
renderGroup (Group m)
  | IM.null m = str "(0)"
  | otherwise = mconcat $ interleave (str "\\oplus") $ map renderComp (IM.toAscList m)
  where
    renderComp (0, p)
      | p > 1     = str "\\Z" <> str "^{\\oplus" <> str (show p) <> str "}"
      | otherwise  = str "\\Z"
    renderComp (l, p) =
      let base = str "\\Z/2" <> (if l > 1 then str "^{" <> str (show l) <> str "}" else mempty)
          paren = p > 1
          wrapped = if paren then str "(" <> base <> str ")" else base
      in wrapped <> (if p > 1 then str "^{\\oplus" <> str (show p) <> str "}" else mempty)
    interleave _ []     = []
    interleave _ [x]    = [x]
    interleave sep (x:xs) = x : sep : interleave sep xs

renderGenerators :: [AdmissibleSeq] -> Int -> Int -> Builder
renderGenerators gens n s = mconcat
  [ str "\\tablehead{\\hline%\n"
  , str "\tDegree &Genus &Generator\\\\%\n"
  , str "\t\\hline &&\\\\}\n"
  , str "\\tabletail{\\hline%\n"
  , str "\t\\multicolumn{3}{r}{%\n"
  , str "\t\\small\\slshape to be continued on the next page}\\\\}\n"
  , str "\\tablelasttail{\\hline}\n"
  , str "\\begin{supertabular}{|c|c|p{9cm}|}\n"
  , mconcat (map renderGenRow gens)
  , str "&&\\\\"
  , str "\\end{supertabular}\n"
  ]
  where
    oneSeq = AdmissibleSeq [1]

    renderGenRow sq =
      let sd = stableDegree sq
          s' = if sd == 0 then s else 1
          w  = gFunction sq n s'
          secondWord = if sd == 0
                       then renderWord (gFunction oneSeq n s) s
                       else renderWord (wordFirstCech w) 1
      in mconcat
        [ str "$", str (show (sd + n)), str "$\n"
        , str "&$", str (show (genus sq)), str "$ &$("
        , renderWord w s'
        , str ","
        , secondWord
        , str ")$\\\\\n"
        ]

renderWord :: Word -> Int -> Builder
renderWord (Word []) _ = mempty
renderWord (Word pls) s = mconcat (map renderPL pls)
  where
    renderPL (PL Beta2 p)   = str "\\beta_2" <> showPow p
    renderPL (PL Sigma p)   = str "\\sigma" <> showPow p
    renderPL (PL Gamma2 p)  = str "\\gamma_2" <> showPow p
    renderPL (PL Phi2 p)    = str "\\varphi_2" <> showPow p
    renderPL (PL Psi2ToS p) =
      (if s == 1
       then str "\\psi_2"
       else str "\\psi_{2^{" <> str (show s) <> str "}}")
      <> showPow p
    showPow 1 = mempty
    showPow p = str "^{" <> str (show p) <> str "}"

str :: String -> Builder
str = fromString
