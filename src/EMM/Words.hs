module EMM.Words
  ( Letter(..)
  , PoweredLetter(..)
  , Word(..)
  , concatLetter
  , gFunction
  , wordFirstCech
  ) where

import Prelude hiding (Word)
import EMM.AdmissibleSequences

data Letter = Beta2 | Sigma | Gamma2 | Phi2 | Psi2ToS
  deriving (Eq, Show)

data PoweredLetter = PL !Letter !Int
  deriving (Eq, Show)

newtype Word = Word { getWord :: [PoweredLetter] }
  deriving (Eq, Show)

emptyWord :: Word
emptyWord = Word []

concatLetter :: Letter -> Int -> Word -> Word
concatLetter _ 0 w = w
concatLetter l p (Word []) = Word [PL l p]
concatLetter l p (Word pls)
  | PL l' p' <- last pls, l' == l = Word (init pls ++ [PL l (p' + p)])
  | otherwise = Word (pls ++ [PL l p])

-- | The g-function from the C++ code.
--   Converts an admissible sequence into a word of Steenrod operations.
gFunction :: AdmissibleSeq -> Int -> Int -> Word
gFunction seq0 n _s = go seq0 n emptyWord
  where
    go s nn w =
      let a0 = firstEntry s
          e  = excess s
      in
        if a0 <= 0 then
          concatLetter Sigma nn w
        else if a0 == 1 then
          let w' = if nn >= 2 then concatLetter Sigma (nn - 1) w else w
          in concatLetter Psi2ToS 1 w'
        else if even a0 && e < nn then
          let w1 = concatLetter Beta2 1 w
              w2 = if nn - e - 1 > 0
                   then concatLetter Sigma (nn - e - 1) w1
                   else w1
              w3 = concatLetter Phi2 1 w2
          in go (firstCech s) e w3
        else if even a0 && e == nn then
          let w1 = concatLetter Gamma2 1 w
          in go (firstCech s) nn w1
        else -- odd a0
          let w1 = if nn - e > 0
                   then concatLetter Sigma (nn - e) w
                   else w
              w2 = concatLetter Phi2 1 w1
          in go (firstCech s) (e - 1) w2

wordFirstCech :: Word -> Word
wordFirstCech (Word [])     = emptyWord
wordFirstCech (Word (_:xs)) = Word xs
