-- |
-- Module      : Main
-- Description : Entry point for the GEMM.
--
-- Dispatches between TUI mode (default, no arguments) and CLI mode
-- (with arguments).  The TUI provides an interactive terminal
-- interface; the CLI supports LaTeX, JSON, and certificate output.
module Main where

import System.Environment (getArgs)
import GEMM.CLI (runCLI)
import TUI (runTUI)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runTUI
    _  -> runCLI args
