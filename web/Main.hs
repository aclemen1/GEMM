-- |
-- Module      : Main
-- Description : Entry point for the GEMM web server.
module Main where

import System.Environment (lookupEnv)
import Server (runServer)

main :: IO ()
main = do
  portStr <- lookupEnv "PORT"
  let port = maybe 3000 read portStr
  putStrLn $ "GEMM web server starting on port " ++ show port
  runServer port
