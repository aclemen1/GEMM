module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import qualified Data.Text.Lazy.IO as TL

import EMM.Types (anticonnexity, groupInDegree, showGroup)
import EMM.GradedGroups (universalCoefficients)
import EMM.EilenbergMacLane (emHomologyWithGenerators)
import EMM.LaTeX (renderDocument)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []        -> interactiveMode
    [a, b, c] -> cliMode (read a) (read b) (read c)
    _         -> putStrLn "Invalid arguments. Usage: emm [s] [n] [range]"

interactiveMode :: IO ()
interactiveMode = do
  putStrLn "\nThe Eilenberg-MacLane machine"
  putStrLn "============================="
  putStrLn "Version 3.0 (Haskell rewrite)\n"
  putStrLn "Alain Clement"
  putStrLn "University of Lausanne"
  putStrLn "Switzerland\n"
  putStrLn "This program computes the integral homology and"
  putStrLn "cohomology groups of Eilenberg-MacLane spaces"
  putStrLn "associated to cyclic groups of order a power of 2,"
  putStrLn "namely spaces of the form K(Z/2^s,n).\n"
  putStrLn "If you want a LaTeX file as output, you can"
  putStrLn "also quit and execute the machine again with the"
  putStrLn "following arguments: emm [s] [n] [range]\n"
  loop

loop :: IO ()
loop = do
  putStr "s (0 to quit) ............: "
  hFlush stdout
  s <- readLn
  if s == (0 :: Int) then return ()
  else do
    putStr "n ........................: "
    hFlush stdout
    n <- readLn
    putStr "range (0 to ?) ...........: "
    hFlush stdout
    range_ <- readLn

    let (homology, gens) = emHomologyWithGenerators s n range_
        cohomology = universalCoefficients homology
        ac = anticonnexity homology

    putStrLn ""
    mapM_ (\deg ->
        putStrLn $ show deg ++ ": " ++ showGroup (groupInDegree deg homology)
      ) [0..ac]
    putStrLn ""

    putStrLn "Results are outputed in the LaTeX file \"output.tex\"."
    putStrLn "Use your preferred LaTeX environment to compile this file.\n"

    TL.writeFile "output.tex" (renderDocument s n homology cohomology gens)
    loop

cliMode :: Int -> Int -> Int -> IO ()
cliMode s n range_ = do
  let (homology, gens) = emHomologyWithGenerators s n range_
      cohomology = universalCoefficients homology
  TL.putStr (renderDocument s n homology cohomology gens)
