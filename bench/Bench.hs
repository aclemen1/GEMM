-- | Benchmarks for the GEMM algorithm.
--
-- Measures wall-clock time for computing H_*(K(Z/p^f, n); Z) across
-- increasing ranges R, to extract empirical complexity exponents.

module Main (main) where

import Test.Tasty.Bench
import qualified Data.IntMap.Strict as IM
import GEMM.Types (GradedGroup(..))
import GEMM.EilenbergMacLane (emHomologyP, emHomologyZ)

-- | Force evaluation by computing the size of the graded group.
forceGG :: GradedGroup -> Int
forceGG (GradedGroup m) = IM.size m

-- | Build a benchmark for K(Z/p^f, n) at range R.
benchPf :: Int -> Int -> Int -> Int -> Benchmark
benchPf p f n r =
  bench name $ whnf (forceGG . emHomologyP p f n) r
  where
    name = "K(Z/" ++ show p ++ "^" ++ show f ++ "," ++ show n
           ++ ") R=" ++ show r

-- | Build a benchmark for K(Z, n) at range R.
benchZ :: Int -> Int -> Benchmark
benchZ n r =
  bench name $ whnf (forceGG . emHomologyZ n) r
  where
    name = "K(Z," ++ show n ++ ") R=" ++ show r

main :: IO ()
main = defaultMain
  [ -- ── beta = 1: single elementary complex ──────────────────────
    -- alpha_th = 3.0 (but effective cost is near-linear for p >= 3)

    bgroup "p=2,f=1,n=2 (beta=1)"
      [ benchPf 2 1 2 r | r <- [50, 100, 200, 400, 800] ]

  , bgroup "p=3,f=1,n=2 (beta=1)"
      [ benchPf 3 1 2 r | r <- [50, 100, 200, 400, 800] ]

  , bgroup "p=5,f=1,n=2 (beta=1)"
      [ benchPf 5 1 2 r | r <- [50, 100, 200, 400, 800] ]

  , bgroup "p=7,f=1,n=2 (beta=1)"
      [ benchPf 7 1 2 r | r <- [50, 100, 200, 400, 800] ]

  , bgroup "p=3,f=1,n=3 (beta=1)"
      [ benchPf 3 1 3 r | r <- [50, 100, 200, 400, 800] ]

    -- ── f > 1: verify f does not change exponent ─────────────────

  , bgroup "p=2,f=2,n=2 (beta=1)"
      [ benchPf 2 2 2 r | r <- [50, 100, 200, 400, 800] ]

  , bgroup "p=2,f=3,n=2 (beta=1)"
      [ benchPf 2 3 2 r | r <- [50, 100, 200, 400, 800] ]

  , bgroup "p=3,f=2,n=2 (beta=1)"
      [ benchPf 3 2 2 r | r <- [50, 100, 200, 400, 800] ]

    -- ── beta = 2: two-factor Kuenneth interaction ────────────────
    -- alpha_th = 4.0 (p=2) or 3.63 (p=3)

  , bgroup "p=2,f=1,n=3 (beta=2)"
      [ benchPf 2 1 3 r | r <- [20, 40, 80, 160] ]

  , bgroup "p=2,f=1,n=4 (beta=2)"
      [ benchPf 2 1 4 r | r <- [10, 20, 40, 80] ]

  , bgroup "p=3,f=1,n=4 (beta=2)"
      [ benchPf 3 1 4 r | r <- [20, 40, 80, 160] ]

  , bgroup "p=2,f=2,n=3 (beta=2)"
      [ benchPf 2 2 3 r | r <- [20, 40, 80, 160] ]

    -- ── beta = 3: higher branching ───────────────────────────────
    -- alpha_th ~ 4.58 (p=2, n=5)

  , bgroup "p=2,f=1,n=5 (beta=3)"
      [ benchPf 2 1 5 r | r <- [10, 20, 40, 80] ]

    -- ── beta = 4: even higher branching ──────────────────────────
    -- alpha_th = 5.0 (p=2, n=7)

  , bgroup "p=2,f=1,n=7 (beta=4)"
      [ benchPf 2 1 7 r | r <- [10, 20, 40] ]

    -- ── K(Z, n): multi-prime computation ─────────────────────────

  , bgroup "K(Z,2)"
      [ benchZ 2 r | r <- [50, 100, 200, 400, 800] ]

  , bgroup "K(Z,3)"
      [ benchZ 3 r | r <- [20, 40, 80, 160] ]

  , bgroup "K(Z,4)"
      [ benchZ 4 r | r <- [10, 20, 40, 80] ]
  ]
