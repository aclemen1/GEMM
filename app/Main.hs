-- |
-- Module      : Main
-- Description : Entry point for the Eilenberg-MacLane Machine.
--
-- Supports three modes of operation:
--
-- == Interactive mode (no arguments)
--
-- Prompts for parameters and displays results, writing LaTeX to @output.tex@.
--
-- == CLI mode for K(Z/p^f, n)
--
-- @emm p f n range@ — outputs LaTeX to stdout.
-- For backward compatibility, @emm s n range@ (3 args) is interpreted
-- as @emm 2 s n range@ (i.e., p=2).
--
-- == CLI mode for K(Z, n)
--
-- @emm Z n range@ — computes K(Z, n).
module Main where

import System.Environment (getArgs)
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

data Opts = Opts
  { optJson :: Bool
  , optTime :: Bool
  }

parseOpts :: [String] -> (Opts, [String])
parseOpts = go (Opts False False)
  where
    go opts ("--json":xs) = go (opts { optJson = True }) xs
    go opts ("--time":xs) = go (opts { optTime = True }) xs
    go opts xs            = (opts, xs)

main :: IO ()
main = do
  args <- getArgs
  let (opts, rest) = parseOpts args
  case rest of
    []              -> interactiveMode
    -- K(Z, n): emm [--json] [--time] Z n range
    ["Z", nS, rS]  -> cliModeZ opts (read nS) (read rS)
    -- Backward compatible: emm [--json] [--time] s n range  =>  p=2, f=s
    [sS, nS, rS]   -> cliModeP opts 2 (read sS) (read nS) (read rS)
    -- General: emm [--json] [--time] p f n range
    [pS, fS, nS, rS] -> cliModeP opts (read pS) (read fS) (read nS) (read rS)
    _               -> do
      putStrLn "Usage:"
      putStrLn "  gemm                                — interactive mode"
      putStrLn "  gemm [--json] [--time] s n range    — K(Z/2^s, n)"
      putStrLn "  gemm [--json] [--time] p f n range  — K(Z/p^f, n)"
      putStrLn "  gemm [--json] [--time] Z n range    — K(Z, n)"

interactiveMode :: IO ()
interactiveMode = do
  putStrLn "\nThe Generalized Eilenberg-MacLane Machine (GEMM)"
  putStrLn "================================================"
  putStrLn "Version 4.0\n"
  putStrLn "Alain Clément"
  putStrLn "University of Lausanne"
  putStrLn "Switzerland\n"
  putStrLn "This program computes the integral homology and"
  putStrLn "cohomology groups of Eilenberg-MacLane spaces"
  putStrLn "K(Z/p^f, n) for any prime p, and K(Z, n).\n"
  putStrLn "If you want a LaTeX file as output, you can"
  putStrLn "also quit and execute the machine again with"
  putStrLn "arguments: gemm [p] [f] [n] [range]\n"
  loop

loop :: IO ()
loop = do
  putStr "p (prime, 0 for Z, -1 to quit): "
  hFlush stdout
  p <- readLn :: IO Int
  if p == -1 then return ()
  else if p == 0 then do
    -- K(Z, n) mode
    putStr "n ........................: "
    hFlush stdout
    n <- readLn
    putStr "range (0 to ?) ...........: "
    hFlush stdout
    range_ <- readLn

    let (homologyFull, primeGens) = emHomologyZWithGenerators n range_
        homology = truncateGraded range_ homologyFull
        cohomology = universalCoefficients homology

    putStrLn ""
    mapM_ (\deg ->
        putStrLn $ show deg ++ ": " ++ showGroup (groupInDegree deg homology)
      ) [0..range_]
    putStrLn ""

    putStrLn "Results are output in the LaTeX file \"output.tex\"."
    putStrLn "Use your preferred LaTeX environment to compile this file.\n"

    TL.writeFile "output.tex" (renderDocumentZ n homology cohomology primeGens)
    loop
  else do
    -- K(Z/p^f, n) mode
    putStr "f (log-order) ............: "
    hFlush stdout
    f <- readLn
    putStr "n ........................: "
    hFlush stdout
    n <- readLn
    putStr "range (0 to ?) ...........: "
    hFlush stdout
    range_ <- readLn

    let (homologyFull, gens) = emHomologyPWithGenerators p f n range_
        homology = truncateGraded range_ homologyFull
        cohomology = universalCoefficients homology

    putStrLn ""
    mapM_ (\deg ->
        putStrLn $ show deg ++ ": " ++ showGroup (groupInDegree deg homology)
      ) [0..range_]
    putStrLn ""

    putStrLn "Results are output in the LaTeX file \"output.tex\"."
    putStrLn "Use your preferred LaTeX environment to compile this file.\n"

    TL.writeFile "output.tex" (renderDocumentP p f n homology cohomology gens)
    loop

-- | Measure CPU time of a fully-evaluated computation, report on stderr.
-- When timing is disabled, just run the action.
timed :: Bool -> String -> IO a -> IO a
timed False _ action = action
timed True label action = do
  t0 <- getCPUTime
  result <- action
  t1 <- getCPUTime
  let ms = fromIntegral (t1 - t0) / (1e9 :: Double)
  hPutStrLn stderr $ label ++ ": " ++ formatMs ms
  return result

-- | Format milliseconds as "X.Y ms" or "X.YYY s".
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
