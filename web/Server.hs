{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Server
-- Description : WAI/Warp web server for the GEMM.
module Server (runServer) where

import Control.DeepSeq (force)
import Control.Exception (evaluate, SomeException, try)
import Data.Maybe (fromMaybe)
import System.Timeout (timeout)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types

import GEMM.Types (truncateGraded)
import GEMM.GradedGroups (universalCoefficients)
import GEMM.EilenbergMacLane
  ( emHomologyPWithGenerators, emHomologyZWithGenerators )
import GEMM.LaTeX (renderDocumentP, renderDocumentZ)
import GEMM.JSON (renderJsonP, renderJsonZ)
import GEMM.Certificate (computeCertificateP, certificateToLean)

import Cache
import Templates

maxRange :: Int
maxRange = 150

timeoutMicros :: Int
timeoutMicros = 60 * 1000000  -- 60 seconds

runServer :: Int -> IO ()
runServer port = do
  cache <- newCache 50
  run port (application cache)

application :: Cache -> Application
application cache req respond = case (requestMethod req, pathInfo req) of
  ("GET",  [])          -> respond $ htmlResponse ok200 (TL.pack indexPage)
  ("GET",  [""])        -> respond $ htmlResponse ok200 (TL.pack indexPage)
  ("POST", ["compute"]) -> do
    body <- parseBody req
    handleCompute cache body respond
  ("GET",  ["export", fmt]) -> do
    let qs = queryString req
    handleExport cache (TL.unpack (TL.fromStrict fmt)) qs respond
  _ -> respond $ htmlResponse notFound404 (TL.pack "Not found.")

-- | Parse URL-encoded form body.
parseBody :: Request -> IO [(BS.ByteString, BS.ByteString)]
parseBody req = do
  body <- strictRequestBody req
  return $ parseSimpleQuery (LBS.toStrict body)

-- | Lookup a query/form parameter.
lookupParam :: BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> Maybe String
lookupParam key pairs = case lookup key pairs of
  Just v  -> Just (bsToStr v)
  Nothing -> Nothing

lookupQS :: BS.ByteString -> Query -> Maybe String
lookupQS key qs = case lookup key qs of
  Just (Just v) -> Just (bsToStr v)
  _             -> Nothing

bsToStr :: BS.ByteString -> String
bsToStr = TL.unpack . TLE.decodeUtf8 . LBS.fromStrict

-- | Parse and validate parameters from form data or query string.
data Params = Params
  { pSpace :: SpaceType
  , pP     :: Int
  , pF     :: Int
  , pN     :: Int
  , pRange :: Int
  }

parseParamsFrom :: (BS.ByteString -> Maybe String) -> Either String Params
parseParamsFrom lk =
  let sp    = fromMaybe "P" (lk "space")
      space = if sp == "Z" then SpaceZ else SpacePF
      p     = maybe 2 read (lk "p")
      f     = maybe 1 read (lk "f")
      n     = maybe 2 read (lk "n")
      r     = maybe 20 read (lk "range")
  in validate space p f n r

validate :: SpaceType -> Int -> Int -> Int -> Int -> Either String Params
validate sp p f n r
  | r > maxRange = Left $ "Range too large (max " ++ show maxRange ++ ")."
  | r < 1        = Left "Range must be >= 1."
  | n < 1        = Left "Dimension n must be >= 1."
  | r < n        = Left "Range must be >= n."
  | sp == SpacePF && p < 2  = Left "Prime p must be >= 2."
  | sp == SpacePF && f < 1  = Left "Exponent f must be >= 1."
  | sp == SpacePF && not (isPrime p) = Left $ show p ++ " is not prime."
  | otherwise = Right (Params sp p f n r)

isPrime :: Int -> Bool
isPrime k
  | k < 2     = False
  | k == 2    = True
  | even k    = False
  | otherwise = all (\d -> k `mod` d /= 0) [3, 5 .. isqrt k]
  where isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

-- | Handle POST /compute
handleCompute :: Cache -> [(BS.ByteString, BS.ByteString)] -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleCompute cache body respond = do
  let ep = parseParamsFrom (\k -> lookupParam k body)
  case ep of
    Left err -> respond $ htmlResponse ok200 (TL.pack $ errorFragment err)
    Right params -> do
      let key = CacheKey (pSpace params) (pP params) (pF params) (pN params) (pRange params)
      cached <- lookupCache cache key
      result <- case cached of
        Just r  -> return (Right r)
        Nothing -> computeWithTimeout params cache key
      case result of
        Left err -> respond $ htmlResponse ok200 (TL.pack $ errorFragment err)
        Right cr -> respond $ htmlResponse ok200 (TL.pack $ resultFragment (pSpace params) cr)

-- | Handle GET /export/:fmt
handleExport :: Cache -> String -> Query -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleExport cache fmt qs respond = do
  let ep = parseParamsFrom (\k -> lookupQS k qs)
  case ep of
    Left err -> respond $ textResponse badRequest400 (TL.pack err)
    Right params -> do
      let key = CacheKey (pSpace params) (pP params) (pF params) (pN params) (pRange params)
      cached <- lookupCache cache key
      cr <- case cached of
        Just r  -> return (Just r)
        Nothing -> do
          result <- computeWithTimeout params cache key
          case result of
            Left _  -> return Nothing
            Right r -> return (Just r)
      case cr of
        Nothing -> respond $ textResponse serviceUnavailable503 "Computation timed out."
        Just r  -> serveExport fmt (pSpace params) r respond

computeWithTimeout :: Params -> Cache -> CacheKey -> IO (Either String ComputedResult)
computeWithTimeout params cache key = do
  mr <- timeout timeoutMicros $ try $ evaluate $ force $ doCompute params
  case mr of
    Nothing        -> return $ Left "Computation timed out. Try a smaller range."
    Just (Left e)  -> return $ Left $ "Computation error: " ++ show (e :: SomeException)
    Just (Right cr) -> do
      insertCache cache key cr
      return (Right cr)

doCompute :: Params -> ComputedResult
doCompute params = case pSpace params of
  SpacePF ->
    let p = pP params; f = pF params; n = pN params; r = pRange params
        (homFull, gens) = emHomologyPWithGenerators p f n r
        hom = truncateGraded r homFull
        coh = universalCoefficients hom
    in ComputedResult hom coh p f n r gens []
  SpaceZ ->
    let n = pN params; r = pRange params
        (homFull, gens) = emHomologyZWithGenerators n r
        hom = truncateGraded r homFull
        coh = universalCoefficients hom
    in ComputedResult hom coh 0 0 n r [] gens

serveExport :: String -> SpaceType -> ComputedResult -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serveExport "latex" sp cr respond = do
  let content = case sp of
        SpacePF -> renderDocumentP (crP cr) (crF cr) (crN cr) (crHomology cr) (crCohomology cr) (crGensP cr)
        SpaceZ  -> renderDocumentZ (crN cr) (crHomology cr) (crCohomology cr) (crGensZ cr)
      fname = exportFilename sp cr "tex"
  respond $ downloadResponse "application/x-latex" fname (TLE.encodeUtf8 content)

serveExport "json" sp cr respond = do
  let content = case sp of
        SpacePF -> renderJsonP (crP cr) (crF cr) (crN cr) (crHomology cr) (crCohomology cr) (crGensP cr)
        SpaceZ  -> renderJsonZ (crN cr) (crHomology cr) (crCohomology cr) (crGensZ cr)
      fname = exportFilename sp cr "json"
  respond $ downloadResponse "application/json" fname (TLE.encodeUtf8 $ TL.pack content)

serveExport "cert" sp cr respond
  | sp == SpacePF = do
      let cert = computeCertificateP (crP cr) (crF cr) (crN cr) (crRange cr)
          content = certificateToLean cert
          fname = exportFilename sp cr "lean"
      respond $ downloadResponse "text/plain; charset=utf-8" fname (TLE.encodeUtf8 $ TL.pack content)
  | otherwise =
      respond $ textResponse badRequest400 "Certificates are only available for K(Z/p^f, n)."

serveExport _ _ _ respond =
  respond $ textResponse badRequest400 "Unknown format. Use latex, json, or cert."

exportFilename :: SpaceType -> ComputedResult -> String -> String
exportFilename sp cr ext = case sp of
  SpacePF -> "gemm-K_Z_p" ++ show (crP cr) ++ "_f" ++ show (crF cr)
              ++ "_n" ++ show (crN cr) ++ "_r" ++ show (crRange cr) ++ "." ++ ext
  SpaceZ  -> "gemm-K_Z_n" ++ show (crN cr) ++ "_r" ++ show (crRange cr) ++ "." ++ ext

-- | Response helpers
htmlResponse :: Status -> TL.Text -> Response
htmlResponse st body = responseLBS st
  [(hContentType, "text/html; charset=utf-8")]
  (TLE.encodeUtf8 body)

textResponse :: Status -> TL.Text -> Response
textResponse st body = responseLBS st
  [(hContentType, "text/plain; charset=utf-8")]
  (TLE.encodeUtf8 body)

downloadResponse :: BS.ByteString -> String -> LBS.ByteString -> Response
downloadResponse ct fname body = responseLBS ok200
  [ (hContentType, ct)
  , ("Content-Disposition", strToBS $ "attachment; filename=\"" ++ fname ++ "\"")
  ]
  body

strToBS :: String -> BS.ByteString
strToBS = LBS.toStrict . TLE.encodeUtf8 . TL.pack
