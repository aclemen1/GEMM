{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : GEMM.FFI
-- Description : C FFI bindings for the GEMM library.
--
-- Exports pure computation functions as C-callable symbols.
-- Every returned 'CString' must be freed exactly once via 'gemm_free'.
--
-- Callers must call @gemm_init()@ before any computation and
-- @gemm_shutdown()@ when done. These are provided by @cbits\/gemm_init.c@.
module GEMM.FFI where

import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, nullPtr)
import Control.Exception (SomeException, try, evaluate)
import Control.DeepSeq (force)
import qualified Data.Text.Lazy as TL

import GEMM.Types (truncateGraded)
import GEMM.GradedGroups (universalCoefficients)
import GEMM.EilenbergMacLane
  ( emHomologyPWithGenerators, emHomologyZWithGenerators )
import GEMM.JSON (renderJsonP, renderJsonZ)
import GEMM.LaTeX (renderDocumentP, renderDocumentZ)
import GEMM.Certificate (computeCertificateP, certificateToLean)

-- | Compute homology/cohomology of K(Z/p^f, n) and return JSON.
foreign export ccall gemm_homology_p :: CInt -> CInt -> CInt -> CInt -> IO CString
gemm_homology_p :: CInt -> CInt -> CInt -> CInt -> IO CString
gemm_homology_p cp cf cn cr = toResult $ do
  let p = fromIntegral cp; f = fromIntegral cf
      n = fromIntegral cn; r = fromIntegral cr
      (homFull, gens) = emHomologyPWithGenerators p f n r
      hom = truncateGraded r homFull
      coh = universalCoefficients hom
  return (renderJsonP p f n hom coh gens)

-- | Compute homology/cohomology of K(Z, n) and return JSON.
foreign export ccall gemm_homology_z :: CInt -> CInt -> IO CString
gemm_homology_z :: CInt -> CInt -> IO CString
gemm_homology_z cn cr = toResult $ do
  let n = fromIntegral cn; r = fromIntegral cr
      (homFull, gens) = emHomologyZWithGenerators n r
      hom = truncateGraded r homFull
      coh = universalCoefficients hom
  return (renderJsonZ n hom coh gens)

-- | Generate a Lean 4 proof certificate for K(Z/p^f, n).
foreign export ccall gemm_certificate :: CInt -> CInt -> CInt -> CInt -> IO CString
gemm_certificate :: CInt -> CInt -> CInt -> CInt -> IO CString
gemm_certificate cp cf cn cr = toResult $ do
  let p = fromIntegral cp; f = fromIntegral cf
      n = fromIntegral cn; r = fromIntegral cr
      cert = computeCertificateP p f n r
  return (certificateToLean cert)

-- | Generate a LaTeX document for K(Z/p^f, n).
foreign export ccall gemm_latex_p :: CInt -> CInt -> CInt -> CInt -> IO CString
gemm_latex_p :: CInt -> CInt -> CInt -> CInt -> IO CString
gemm_latex_p cp cf cn cr = toResult $ do
  let p = fromIntegral cp; f = fromIntegral cf
      n = fromIntegral cn; r = fromIntegral cr
      (homFull, gens) = emHomologyPWithGenerators p f n r
      hom = truncateGraded r homFull
      coh = universalCoefficients hom
  return (TL.unpack (renderDocumentP p f n hom coh gens))

-- | Generate a LaTeX document for K(Z, n).
foreign export ccall gemm_latex_z :: CInt -> CInt -> IO CString
gemm_latex_z :: CInt -> CInt -> IO CString
gemm_latex_z cn cr = toResult $ do
  let n = fromIntegral cn; r = fromIntegral cr
      (homFull, gens) = emHomologyZWithGenerators n r
      hom = truncateGraded r homFull
      coh = universalCoefficients hom
  return (TL.unpack (renderDocumentZ n hom coh gens))

-- | Free a string returned by any gemm_* function.
foreign export ccall gemm_free :: Ptr a -> IO ()
gemm_free :: Ptr a -> IO ()
gemm_free p
  | p == nullPtr = return ()
  | otherwise    = free p

-- | Run a computation, catch exceptions, force the result, return CString.
toResult :: IO String -> IO CString
toResult action = do
  result <- try (action >>= evaluate . force)
  case result of
    Right s -> newCString s
    Left e  -> newCString ("{\"error\":" ++ show (show (e :: SomeException)) ++ "}")
