-- |
-- Module      : Main
-- Description : Entry point for the GEMM.
--
-- Dispatches between TUI mode (default, no arguments) and CLI mode
-- (with arguments).  The TUI provides an interactive terminal
-- interface; the CLI supports LaTeX, JSON, and certificate output.
module Main where

import System.Environment (getArgs)
import Data.Version (showVersion)
import Paths_gemm (version)
import GEMM.CLI (runCLI)
import TUI (runTUI)

-- | Version string derived from gemm.cabal (single source of truth).
versionStr :: String
versionStr = showVersion version

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runTUI versionStr
    _  -> runCLI versionStr args
