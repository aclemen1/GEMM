-- |
-- Module      : GEMM.CLI
-- Description : Command-line interface for the GEMM.
--
-- Handles argument parsing and dispatches to the appropriate computation
-- mode: homology (LaTeX/JSON), or certificate generation.
module GEMM.CLI
  ( runCLI
  ) where

import System.IO (hFlush, stdout, stderr, hPutStrLn)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Text.Lazy.IO as TL

import GEMM.Types (groupInDegree, showGroup, truncateGraded)
import GEMM.GradedGroups (universalCoefficients)
import GEMM.EilenbergMacLane
  ( emHomologyPWithGenerators, emHomologyZWithGenerators )
import GEMM.LaTeX (renderDocumentP, renderDocumentZ)
import GEMM.JSON (renderJsonP, renderJsonZ)
import GEMM.Certificate (computeCertificateP, certificateToLean)
import GEMM.Schema (runSchema)

data Opts = Opts
  { optJson :: Bool
  , optTime :: Bool
  , optCert :: Bool
  , optName :: Maybe String
  }

defaultOpts :: Opts
defaultOpts = Opts False False False Nothing

parseOpts :: [String] -> (Opts, [String])
parseOpts = go defaultOpts
  where
    go opts ("--json":xs)    = go (opts { optJson = True }) xs
    go opts ("--time":xs)    = go (opts { optTime = True }) xs
    go opts ("--cert":xs)    = go (opts { optCert = True }) xs
    go opts ("--name":n:xs)  = go (opts { optName = Just n }) xs
    go opts xs               = (opts, xs)

-- | Run the CLI with the given arguments.
runCLI :: [String] -> IO ()
runCLI args = do
  case args of
    ("schema":rest) -> runSchema rest
    _ -> runCLI' args

-- | Run the CLI after schema dispatch.
runCLI' :: [String] -> IO ()
runCLI' args = do
  let (opts, rest) = parseOpts args
  case rest of
    -- K(Z, n): gemm [flags] Z n range
    ["Z", nS, rS]  | optCert opts ->
      hPutStrLn stderr "Error: certificates not supported for K(Z, n) yet."
    ["Z", nS, rS]  -> cliModeZ opts (read nS) (read rS)
    -- Backward compatible: gemm [flags] s n range  =>  p=2, f=s
    [sS, nS, rS]   | optCert opts ->
      cliModeCert opts 2 (read sS) (read nS) (read rS)
    [sS, nS, rS]   -> cliModeP opts 2 (read sS) (read nS) (read rS)
    -- General: gemm [flags] p f n range
    [pS, fS, nS, rS] | optCert opts ->
      cliModeCert opts (read pS) (read fS) (read nS) (read rS)
    [pS, fS, nS, rS] -> cliModeP opts (read pS) (read fS) (read nS) (read rS)
    _ -> usage

usage :: IO ()
usage = do
  putStrLn "The Generalized Eilenberg-MacLane Machine (GEMM)"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  gemm                                    — TUI mode"
  putStrLn "  gemm [flags] s n range                  — K(Z/2^s, n)"
  putStrLn "  gemm [flags] p f n range                — K(Z/p^f, n)"
  putStrLn "  gemm [flags] Z n range                  — K(Z, n)"
  putStrLn "  gemm schema [command]                   — introspect commands (JSON)"
  putStrLn ""
  putStrLn "Flags:"
  putStrLn "  --json           Output JSON instead of LaTeX"
  putStrLn "  --time           Show timing on stderr"
  putStrLn "  --cert           Generate Lean 4 proof certificate"
  putStrLn "  --name NAME      Certificate definition name"

-- | Measure CPU time of a fully-evaluated computation.
timed :: Bool -> String -> IO a -> IO a
timed False _ action = action
timed True label action = do
  t0 <- getCPUTime
  result <- action
  t1 <- getCPUTime
  let ms = fromIntegral (t1 - t0) / (1e9 :: Double)
  hPutStrLn stderr $ label ++ ": " ++ formatMs ms
  return result

formatMs :: Double -> String
formatMs t
  | t < 1000  = roundTo 1 t ++ " ms"
  | otherwise = roundTo 3 (t / 1000) ++ " s"
  where
    roundTo :: Int -> Double -> String
    roundTo d x =
      let factor = (10 :: Int) ^ d
          r = fromIntegral (round (x * fromIntegral factor) :: Int)
              / fromIntegral factor :: Double
      in show r

-- | CLI mode for K(Z/p^f, n): output LaTeX or JSON to stdout.
cliModeP :: Opts -> Int -> Int -> Int -> Int -> IO ()
cliModeP opts p f n range_ = do
  let t = timed (optTime opts)
  (homologyFull, gens) <- t "homology" $
    evaluate (force (emHomologyPWithGenerators p f n range_))
  let homology = truncateGraded range_ homologyFull
  cohomology <- t "cohomology" $
    evaluate (force (universalCoefficients homology))
  if optJson opts
    then putStr (renderJsonP p f n homology cohomology gens)
    else TL.putStr (renderDocumentP p f n homology cohomology gens)

-- | CLI mode for K(Z, n): output LaTeX or JSON to stdout.
cliModeZ :: Opts -> Int -> Int -> IO ()
cliModeZ opts n range_ = do
  let t = timed (optTime opts)
  (homologyFull, primeGens) <- t "homology" $
    evaluate (force (emHomologyZWithGenerators n range_))
  let homology = truncateGraded range_ homologyFull
  cohomology <- t "cohomology" $
    evaluate (force (universalCoefficients homology))
  if optJson opts
    then putStr (renderJsonZ n homology cohomology primeGens)
    else TL.putStr (renderDocumentZ n homology cohomology primeGens)

-- | CLI mode for certificate generation.
cliModeCert :: Opts -> Int -> Int -> Int -> Int -> IO ()
cliModeCert opts p f n range_ = do
  let t = timed (optTime opts)
  cert <- t "certificate" $
    evaluate (force (computeCertificateP p f n range_))
  let lean = certificateToLean cert
      -- Replace default name if --name was specified
      lean' = case optName opts of
        Nothing   -> lean
        Just name -> replaceName "cert" name lean
  putStr lean'

-- | Replace definition name in generated Lean code.
replaceName :: String -> String -> String -> String
replaceName old new = go
  where
    go [] = []
    go s@(c:cs)
      | take (length prefix) s == prefix =
          "def " ++ new ++ go (drop (length prefix) s)
      | take (length ref) s == ref =
          "verifyFull " ++ new ++ go (drop (length ref) s)
      | otherwise = c : go cs
    prefix = "def " ++ old
    ref = "verifyFull " ++ old
