module EMM.AdmissibleSequences
  ( AdmissibleSeq(..)
  , admissible
  , stableDegree
  , excess
  , genus
  , firstEntry
  , lastEntry
  , generateSequences
  , filterByEvenFirst
  , filterByHeight
  , firstCech
  ) where

newtype AdmissibleSeq = AdmissibleSeq { getSeq :: [Int] }
  deriving (Eq, Show)

admissible :: AdmissibleSeq -> Bool
admissible (AdmissibleSeq xs) = go xs
  where
    go (a:b:rest) = a >= 2 * b && go (b:rest)
    go _          = True

stableDegree :: AdmissibleSeq -> Int
stableDegree (AdmissibleSeq []) = -1
stableDegree (AdmissibleSeq xs) = sum xs

excess :: AdmissibleSeq -> Int
excess s@(AdmissibleSeq xs) = 2 * head' xs - stableDegree s
  where head' []    = 0
        head' (a:_) = a

firstEntry :: AdmissibleSeq -> Int
firstEntry (AdmissibleSeq [])    = -1
firstEntry (AdmissibleSeq (x:_)) = x

lastEntry :: AdmissibleSeq -> Int
lastEntry (AdmissibleSeq [])  = -1
lastEntry (AdmissibleSeq xs)  = last xs

genus :: AdmissibleSeq -> Int
genus s
  | a0 == 0 || a0 == 1       = 1
  | even (lastEntry s)        = 2
  | otherwise                 = 3
  where a0 = firstEntry s

-- | Generate all admissible sequences with given stable degree
--   and excess in [minE..maxE].
generateSequences :: Int -> Int -> Int -> [AdmissibleSeq]
generateSequences sd minE maxE =
  concatMap (\e -> if (sd + e) `mod` 2 == 0
                   then iterate' (AdmissibleSeq []) sd e
                   else [])
            [minE..maxE]

iterate' :: AdmissibleSeq -> Int -> Int -> [AdmissibleSeq]
iterate' incoming sd e
  | (sd + e) `mod` 2 /= 0 = []
  | a > sd                 = []
  | sd == 0 && e == 0      = [incoming]
  | otherwise              =
      let newSD = sd - a
      in concatMap (\i ->
           let newE = 2 * i - newSD
           in if newE >= 0
              then iterate' (appendSeq incoming a) newSD newE
              else []
         ) [a `div` 2 - e .. a `div` 2]
  where
    a = (sd + e) `div` 2

appendSeq :: AdmissibleSeq -> Int -> AdmissibleSeq
appendSeq (AdmissibleSeq xs) v = AdmissibleSeq (xs ++ [v])

filterByEvenFirst :: [AdmissibleSeq] -> [AdmissibleSeq]
filterByEvenFirst = filter (\s -> case getSeq s of
                                    []    -> True
                                    (x:_) -> even x)

filterByHeight :: Int -> [AdmissibleSeq] -> [AdmissibleSeq]
filterByHeight height = filter (\s ->
  not (even (firstEntry s) && excess s == height))

firstCech :: AdmissibleSeq -> AdmissibleSeq
firstCech (AdmissibleSeq [])     = AdmissibleSeq []
firstCech (AdmissibleSeq (_:xs)) = AdmissibleSeq xs
