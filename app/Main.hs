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
import System.IO (hFlush, stdout)
import qualified Data.Text.Lazy.IO as TL

import EMM.Types (anticonnexity, groupInDegree, showGroup)
import EMM.GradedGroups (universalCoefficients)
import EMM.EilenbergMacLane
  ( emHomologyPWithGenerators, emHomologyZWithGenerators )
import EMM.LaTeX (renderDocumentP, renderDocumentZ)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []              -> interactiveMode
    -- K(Z, n): emm Z n range
    ["Z", nS, rS]  -> cliModeZ (read nS) (read rS)
    -- Backward compatible: emm s n range  =>  p=2, f=s
    [sS, nS, rS]   -> cliModeP 2 (read sS) (read nS) (read rS)
    -- General: emm p f n range
    [pS, fS, nS, rS] -> cliModeP (read pS) (read fS) (read nS) (read rS)
    _               -> do
      putStrLn "Usage:"
      putStrLn "  emm                   — interactive mode"
      putStrLn "  emm s n range         — K(Z/2^s, n)  (backward compatible)"
      putStrLn "  emm p f n range       — K(Z/p^f, n)  (general prime)"
      putStrLn "  emm Z n range         — K(Z, n)"

interactiveMode :: IO ()
interactiveMode = do
  putStrLn "\nThe Eilenberg-MacLane machine"
  putStrLn "============================="
  putStrLn "Version 4.0 (generalized Haskell rewrite)\n"
  putStrLn "Alain Clement"
  putStrLn "University of Lausanne"
  putStrLn "Switzerland\n"
  putStrLn "This program computes the integral homology and"
  putStrLn "cohomology groups of Eilenberg-MacLane spaces"
  putStrLn "K(Z/p^f, n) for any prime p, and K(Z, n).\n"
  putStrLn "If you want a LaTeX file as output, you can"
  putStrLn "also quit and execute the machine again with"
  putStrLn "arguments: emm [p] [f] [n] [range]\n"
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

    let (homology, primeGens) = emHomologyZWithGenerators n range_
        cohomology = universalCoefficients homology
        ac = anticonnexity homology

    putStrLn ""
    mapM_ (\deg ->
        putStrLn $ show deg ++ ": " ++ showGroup (groupInDegree deg homology)
      ) [0..ac]
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

    let (homology, gens) = emHomologyPWithGenerators p f n range_
        cohomology = universalCoefficients homology
        ac = anticonnexity homology

    putStrLn ""
    mapM_ (\deg ->
        putStrLn $ show deg ++ ": " ++ showGroup (groupInDegree deg homology)
      ) [0..ac]
    putStrLn ""

    putStrLn "Results are output in the LaTeX file \"output.tex\"."
    putStrLn "Use your preferred LaTeX environment to compile this file.\n"

    TL.writeFile "output.tex" (renderDocumentP p f n homology cohomology gens)
    loop

-- | CLI mode for K(Z/p^f, n): output LaTeX to stdout.
cliModeP :: Int -> Int -> Int -> Int -> IO ()
cliModeP p f n range_ = do
  let (homology, gens) = emHomologyPWithGenerators p f n range_
      cohomology = universalCoefficients homology
  TL.putStr (renderDocumentP p f n homology cohomology gens)

-- | CLI mode for K(Z, n): output LaTeX to stdout.
cliModeZ :: Int -> Int -> IO ()
cliModeZ n range_ = do
  let (homology, primeGens) = emHomologyZWithGenerators n range_
      cohomology = universalCoefficients homology
  TL.putStr (renderDocumentZ n homology cohomology primeGens)
