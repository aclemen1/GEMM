-- |
-- Module      : Cache
-- Description : STM-based LRU cache for computed results.
module Cache
  ( Cache
  , CacheKey(..)
  , ComputedResult(..)
  , SpaceType(..)
  , newCache
  , lookupCache
  , insertCache
  ) where

import Control.Concurrent.STM
import Control.DeepSeq (NFData(..))
import qualified Data.Map.Strict as Map

import GEMM.Types (GradedGroup)
import GEMM.AdmissibleSequences (AdmissibleSeq)

data SpaceType = SpacePF | SpaceZ
  deriving (Eq, Ord, Show)

data CacheKey = CacheKey
  { ckSpace :: SpaceType
  , ckP     :: Int
  , ckF     :: Int
  , ckN     :: Int
  , ckRange :: Int
  } deriving (Eq, Ord, Show)

data ComputedResult = ComputedResult
  { crHomology  :: GradedGroup
  , crCohomology :: GradedGroup
  , crP         :: Int
  , crF         :: Int
  , crN         :: Int
  , crRange     :: Int
  , crGensP     :: [AdmissibleSeq]
  , crGensZ     :: [(Int, [AdmissibleSeq])]
  }

instance NFData ComputedResult where
  rnf (ComputedResult h c p f n r gp gz) =
    rnf h `seq` rnf c `seq` rnf p `seq` rnf f `seq`
    rnf n `seq` rnf r `seq` rnf gp `seq` rnf gz

type CacheEntry = (Int, ComputedResult)  -- (insertion order, result)

data Cache = Cache
  { cEntries  :: TVar (Map.Map CacheKey CacheEntry)
  , cCounter  :: TVar Int
  , cMaxSize  :: Int
  }

newCache :: Int -> IO Cache
newCache maxSize = do
  entries <- newTVarIO Map.empty
  counter <- newTVarIO 0
  return Cache { cEntries = entries, cCounter = counter, cMaxSize = maxSize }

lookupCache :: Cache -> CacheKey -> IO (Maybe ComputedResult)
lookupCache cache key = atomically $ do
  m <- readTVar (cEntries cache)
  return $ snd <$> Map.lookup key m

insertCache :: Cache -> CacheKey -> ComputedResult -> IO ()
insertCache cache key result = atomically $ do
  n <- readTVar (cCounter cache)
  let n' = n + 1
  writeTVar (cCounter cache) n'
  modifyTVar' (cEntries cache) $ \m ->
    let m' = Map.insert key (n', result) m
    in if Map.size m' > cMaxSize cache
       then evictOldest m'
       else m'

evictOldest :: Map.Map CacheKey CacheEntry -> Map.Map CacheKey CacheEntry
evictOldest m
  | Map.null m = m
  | otherwise  =
      let oldest = Map.foldlWithKey' findOldest Nothing m
      in case oldest of
           Nothing     -> m
           Just (k, _) -> Map.delete k m
  where
    findOldest Nothing k v = Just (k, fst v)
    findOldest (Just (bk, bn)) k v
      | fst v < bn = Just (k, fst v)
      | otherwise   = Just (bk, bn)
